export declare function deg2hms(deg: number): string;
export declare function deg2dms(deg: number): string;
export declare function hms2deg(hms: string): number;
export declare function dms2deg(dms: string): number;

export declare function dateToLocalObservingNight(date: Date): string;

export declare function parseAttachmentId(maybeAttachmentId: string): string | undefined;
export declare function parseCallForProposalsId(maybeCallForProposalsId: string): string | undefined;
export declare function parseGroupId(maybeGroupId: string): string | undefined;
export declare function parseObservationId(maybeObservationId: string): string | undefined;
export declare function parseProgramId(maybeProgramId: string): string | undefined;
export declare function parseProgramNoteId(maybeProgramNoteId: string): string | undefined;
export declare function parseProgramUserId(maybeProgramUserId: string): string | undefined;
export declare function parseVisitId(maybeVisitId: string): string | undefined;
