export declare function deg2hms(deg: number): string;
export declare function deg2dms(deg: number): string;
export declare function hms2deg(hms: string): number;
export declare function dms2deg(dms: string): number;

export declare function signedArcSeconds(arcseconds: string | number): number;

export declare function dateToLocalObservingNight(date: Date): string;

export declare function parseAttachmentId(maybeAttachmentId: string): string | undefined;
export declare function parseCallForProposalsId(maybeCallForProposalsId: string): string | undefined;
export declare function parseConfigurationRequestId(maybeConfigurationRequestId: string): string | undefined;
export declare function parseDatasetId(maybeDatasetId: string): string | undefined;
export declare function parseExecutionEventId(maybeExecutionEventId: string): string | undefined;
export declare function parseGroupId(maybeGroupId: string): string | undefined;
export declare function parseObservationId(maybeObservationId: string): string | undefined;
export declare function parseProgramId(maybeProgramId: string): string | undefined;
export declare function parseProgramNoteId(maybeProgramNoteId: string): string | undefined;
export declare function parseProgramUserId(maybeProgramUserId: string): string | undefined;
export declare function parseStandardRoleId(maybeStandardRoleId: string): string | undefined;
export declare function parseTargetId(maybeTargetId: string): string | undefined;
export declare function parseUserId(maybeUserId: string): string | undefined;
export declare function parseVisitId(maybeVisitId: string): string | undefined;

export declare function parseDmsString(maybeDms: string): string | undefined;
export declare function parseHmsString(maybeHms: string): string | undefined;
export declare function parseEpochString(maybeEpoch: string): string | undefined;

export declare function toRightAscension(coord: number): RightAscension;
export declare function toDeclination(coord: number): Declination;
export declare function toAngle(coord: number): Angle;
export declare function toProperMotion(pmRa: number, pmDec: number): ProperMotion;

type RightAscension = {
  /** Right Ascension (RA) in degrees */
  readonly degrees: string | number;
  /** Right Ascension (RA) in HH:MM:SS.SSS format */
  readonly hms: string;
  /** Right Ascension (RA) in hours */
  readonly hours: string | number;
  /** Right Ascension (RA) in µs */
  readonly microseconds: bigint;
};

type Declination = {
  /** Declination in signed degrees */
  readonly degrees: string | number;
  /** Declination in DD:MM:SS.SS format */
  readonly dms: string;
  /** Declination in signed µas */
  readonly microarcseconds: bigint;
};

type Angle = {
  /** Angle in amin */
  readonly arcminutes: string | number;
  /** Angle in asec */
  readonly arcseconds: string | number;
  /** Angle in deg */
  readonly degrees: string | number;
  /** Angle in DD:MM:SS */
  readonly dms: string;
  /** Angle in HH:MM:SS */
  readonly hms: string;
  /** Angle in hrs */
  readonly hours: string | number;
  /** Angle in µas */
  readonly microarcseconds: bigint;
  /** Angle in µs */
  readonly microseconds: string | number;
  /** Angle in mas */
  readonly milliarcseconds: string | number;
  /** Angle in ms */
  readonly milliseconds: string | number;
  /** Angle in min */
  readonly minutes: string | number;
  /** Angle in sec */
  readonly seconds: string | number;
};

type ProperMotion = {
  /** Proper motion in declination */
  readonly dec: ProperMotionDeclination;
  /** Proper motion in RA */
  readonly ra: ProperMotionRA;
};

type ProperMotionDeclination = {
  /** Proper motion in properMotion μas/year */
  readonly microarcsecondsPerYear: bigint;
  /** Proper motion in properMotion mas/year */
  readonly milliarcsecondsPerYear: string | number;
};

type ProperMotionRA = {
  /** Proper motion in properMotion μas/year */
  readonly microarcsecondsPerYear: bigint;
  /** Proper motion in properMotion mas/year */
  readonly milliarcsecondsPerYear: string | number;
};
