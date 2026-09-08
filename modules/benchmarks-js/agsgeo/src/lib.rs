// PROTOTYPE kernel for .scratch/ags-wasm ticket 05. Handle-based arena of MultiPolygons.
use geo::{
    Area, BooleanOps, BoundingRect, Contains, Coord, Intersects, LineString, MapCoords,
    MultiPolygon, Point, Polygon,
};
use std::cell::RefCell;
use wasm_bindgen::prelude::*;

thread_local! {
    static ARENA: RefCell<Vec<Option<MultiPolygon<f64>>>> = RefCell::new(Vec::new());
    static FREE: RefCell<Vec<u32>> = RefCell::new(Vec::new());
}

fn put(g: MultiPolygon<f64>) -> u32 {
    if let Some(h) = FREE.with(|f| f.borrow_mut().pop()) {
        ARENA.with(|a| a.borrow_mut()[h as usize] = Some(g));
        return h;
    }
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
        a.push(Some(g));
        (a.len() - 1) as u32
    })
}

fn with<R>(h: u32, f: impl FnOnce(&MultiPolygon<f64>) -> R) -> R {
    ARENA.with(|a| f(a.borrow()[h as usize].as_ref().expect("freed handle")))
}

fn ring(coords: &[f64]) -> MultiPolygon<f64> {
    let pts: Vec<Coord<f64>> = coords
        .chunks_exact(2)
        .map(|c| Coord { x: c[0], y: c[1] })
        .collect();
    MultiPolygon::new(vec![Polygon::new(LineString::new(pts), vec![])])
}

#[wasm_bindgen]
pub fn empty_new() -> u32 {
    put(MultiPolygon::new(vec![]))
}

/// Flat closed ring x0,y0,x1,y1,...
#[wasm_bindgen]
pub fn poly_new(coords: &[f64]) -> u32 {
    put(ring(coords))
}

#[wasm_bindgen]
pub fn rect_new(x0: f64, y0: f64, x1: f64, y1: f64) -> u32 {
    let (xa, xb) = (x0.min(x1), x0.max(x1));
    let (ya, yb) = (y0.min(y1), y0.max(y1));
    put(ring(&[xa, ya, xb, ya, xb, yb, xa, yb, xa, ya]))
}

/// JTS GeometricShapeFactory.createEllipse semantics from a bounding box.
#[wasm_bindgen]
pub fn ellipse_new(x0: f64, y0: f64, x1: f64, y1: f64, npts: u32) -> u32 {
    let rx = (x1 - x0).abs() / 2.0;
    let ry = (y1 - y0).abs() / 2.0;
    let cx = (x0 + x1) / 2.0;
    let cy = (y0 + y1) / 2.0;
    let n = npts as usize;
    let mut v = Vec::with_capacity((n + 1) * 2);
    for i in 0..n {
        let ang = i as f64 * (2.0 * std::f64::consts::PI / n as f64);
        v.push(cx + rx * ang.cos());
        v.push(cy + ry * ang.sin());
    }
    v.push(v[0]);
    v.push(v[1]);
    put(ring(&v))
}

/// JTS GeometricShapeFactory.createArcPolygon semantics.
#[wasm_bindgen]
pub fn arc_new(x0: f64, y0: f64, x1: f64, y1: f64, start: f64, extent: f64, npts: u32) -> u32 {
    let rx = (x1 - x0).abs() / 2.0;
    let ry = (y1 - y0).abs() / 2.0;
    let cx = (x0 + x1) / 2.0;
    let cy = (y0 + y1) / 2.0;
    let two_pi = 2.0 * std::f64::consts::PI;
    let size = if extent <= 0.0 || extent > two_pi { two_pi } else { extent };
    let n = npts as usize;
    let inc = size / (n - 1) as f64;
    let mut v = Vec::with_capacity((n + 2) * 2);
    v.push(cx);
    v.push(cy);
    for i in 0..n {
        let ang = start + inc * i as f64;
        v.push(cx + rx * ang.cos());
        v.push(cy + ry * ang.sin());
    }
    v.push(cx);
    v.push(cy);
    put(ring(&v))
}

/// kind: 0 intersection, 1 union, 2 difference
#[wasm_bindgen]
pub fn op(kind: u32, a: u32, b: u32) -> u32 {
    let r = with(a, |ga| {
        with(b, |gb| match kind {
            0 => ga.intersection(gb),
            1 => ga.union(gb),
            _ => ga.difference(gb),
        })
    });
    put(r)
}

/// x' = m00*x + m01*y + m02 ; y' = m10*x + m11*y + m12
#[wasm_bindgen]
pub fn affine(h: u32, m00: f64, m01: f64, m02: f64, m10: f64, m11: f64, m12: f64) -> u32 {
    let r = with(h, |g| {
        g.map_coords(|Coord { x, y }| Coord {
            x: m00 * x + m01 * y + m02,
            y: m10 * x + m11 * y + m12,
        })
    });
    put(r)
}

#[wasm_bindgen]
pub fn area(h: u32) -> f64 {
    with(h, |g| g.unsigned_area())
}

/// [minx, miny, maxx, maxy]; NaNs when empty
#[wasm_bindgen]
pub fn bbox(h: u32) -> Vec<f64> {
    with(h, |g| match g.bounding_rect() {
        Some(r) => vec![r.min().x, r.min().y, r.max().x, r.max().y],
        None => vec![f64::NAN; 4],
    })
}

#[wasm_bindgen]
pub fn contains_point(h: u32, x: f64, y: f64) -> bool {
    with(h, |g| g.contains(&Point::new(x, y)))
}

#[wasm_bindgen]
pub fn intersects(a: u32, b: u32) -> bool {
    with(a, |ga| with(b, |gb| ga.intersects(gb)))
}

#[wasm_bindgen]
pub fn free(h: u32) {
    ARENA.with(|a| a.borrow_mut()[h as usize] = None);
    FREE.with(|f| f.borrow_mut().push(h));
}

#[wasm_bindgen]
pub fn live() -> u32 {
    ARENA.with(|a| a.borrow().iter().filter(|g| g.is_some()).count() as u32)
}
