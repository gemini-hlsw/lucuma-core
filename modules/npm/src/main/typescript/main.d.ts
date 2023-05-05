declare class PlotPoint {
  constructor(instant: Date, airmass: Number, altitude: Number);
  airmass: Number;
  altitude: Number;
  instant: Date;
}

declare function nightPlot(site: Site, start: Date, coords: String): PlotPoint[];
