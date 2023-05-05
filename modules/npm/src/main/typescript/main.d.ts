export declare class PlotPoint {
  constructor(instant: Date, airmass: number, altitude: number);
  airmass: number;
  altitude: number;
  instant: Date;
}

export declare interface Site {}
export declare const GN: Site;
export declare const GS: Site;

export declare function nightPlot(site: Site, start: Date, coords: string): PlotPoint[];
