declare class PlotPoint {
  constructor(instant: Date, airmass: number, altitude: number);
  airmass: number;
  altitude: number;
  instant: Date;
}

declare function nightPlot(site: Site, start: Date, coords: string): PlotPoint[];
