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

export declare function deg2hms(deg: number): string;
export declare function deg2dms(deg: number): string;
export declare function hms2deg(hms: string): number;
export declare function dms2deg(dms: string): number;
