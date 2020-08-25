
CREATE TABLE e_port_disposition (
    id          identifier PRIMARY KEY,
    short_name  TEXT       NOT NULL
);

ALTER TABLE e_port_disposition OWNER TO postgres;

COPY e_port_disposition (id, short_name) FROM stdin;
Side	Side Looking
Bottom	Up Looking
\.

ALTER TABLE e_gmos_north_fpu
  ADD COLUMN x_offset NUMERIC(5,3) DEFAULT 0.0 NOT NULL;

COMMENT ON COLUMN e_gmos_north_fpu.x_offset IS 'arcsec';

UPDATE e_gmos_north_fpu
  SET x_offset = DEFAULT;

UPDATE e_gmos_north_fpu
  SET x_offset = 33.500 WHERE id = 'Ifu1';

UPDATE e_gmos_north_fpu
  SET x_offset = 31.750 WHERE id = 'Ifu2';

UPDATE e_gmos_north_fpu
  SET x_offset = 35.250 WHERE id = 'Ifu3';

ALTER TABLE e_gmos_south_fpu
  ADD COLUMN x_offset NUMERIC(5,3) DEFAULT 0.0 NOT NULL;

COMMENT ON COLUMN e_gmos_south_fpu.x_offset IS 'arcsec';

UPDATE e_gmos_south_fpu
  SET x_offset = DEFAULT;

UPDATE e_gmos_south_fpu
  SET x_offset = -31.750 WHERE id = 'Ifu1';

UPDATE e_gmos_south_fpu
  SET x_offset = -30.875 WHERE id = 'Ifu2';

UPDATE e_gmos_south_fpu
  SET x_offset = -32.625 WHERE id = 'Ifu3';

UPDATE e_gmos_south_fpu
  SET x_offset = -31.750 WHERE id = 'IfuN';

UPDATE e_gmos_south_fpu
  SET x_offset = -30.875 WHERE id = 'IfuNB';

UPDATE e_gmos_south_fpu
  SET x_offset = -32.625 WHERE id = 'IfuNR';
