-------------------------------
-- Giapi status apply for GHOST
-------------------------------

INSERT INTO e_giapi_status_apply(id, instrument_id, type, status_item, apply_item)
VALUES
	('FiberAgitator', 'Ghost', 'Int', 'ghost:GhostSlitUnitCC.status.fa1.active', 'ghost:cc:slu:fa1.type'),
	('RedExposureTime', 'Ghost', 'Double', 'ghost:GhostRed.target', 'ghost:dc:red.duration'),
	('RedExposureCount', 'Ghost', 'Int', 'ghost:GhostRed.repeat', 'ghost:dc:red.repeat'),
	('RedExposureBinningRcf', 'Ghost', 'Int', 'ghost:GhostRed.Hardware_Status_u.camera_status.rcf', 'ghost:dc:red.rcf'),
	('RedExposureBinningCcf', 'Ghost', 'Int', 'ghost:GhostRed.Hardware_Status_u.camera_status.ccf', 'ghost:dc:red.ccf'),
	('BlueExposureTime', 'Ghost', 'Double', 'ghost:GhostBlue.target', 'ghost:dc:blue.duration'),
	('BlueExposureCount', 'Ghost', 'Int', 'ghost:GhostBlue.repeat', 'ghost:dc:blue.repeat'),
	('BlueExposureBinningRcf', 'Ghost', 'Int', 'ghost:GhostBlue.Hardware_Status_u.camera_status.rcf', 'ghost:dc:blue.rcf'),
	('BlueExposureBinningCcf', 'Ghost', 'Int', 'ghost:GhostBlue.Hardware_Status_u.camera_status.ccf', 'ghost:dc:blue.ccf'),
	('SRIFU1CoordsRADeg', 'Ghost', 'Double', 'ghost:GhostCassegrainUnitCC.status.ifu1.std.fpx', 'ghost:cc:cu:ifu1.fpx'),
	('SRIFU1CoordsDecDeg', 'Ghost', 'Double', 'ghost:GhostCassegrainUnitCC.status.ifu1.std.fpy', 'ghost:cc:cu:ifu1.fpy'),
	('SRIFU1CoordsRAHMS', 'Ghost', 'String', 'ghost:GhostCassegrainUnitCC.status.ifu1.std.ra', 'ghost:cc:cu:ifu1.ra'),
	('SRIFU1CoordsDecDMS', 'Ghost', 'String', 'ghost:GhostCassegrainUnitCC.status.ifu1.std.dec', 'ghost:cc:cu:ifu1.dec'),
	('SRIFU2CoordsRADeg', 'Ghost', 'Double', 'ghost:GhostCassegrainUnitCC.status.ifu2.std.fpx', 'ghost:cc:cu:ifu2.fpx'),
	('SRIFU2CoordsDecDeg', 'Ghost', 'Double', 'ghost:GhostCassegrainUnitCC.status.ifu2.std.fpy', 'ghost:cc:cu:ifu2.fpy'),
	('SRIFU2CoordsRAHMS', 'Ghost', 'String', 'ghost:GhostCassegrainUnitCC.status.ifu2.std.ra', 'ghost:cc:cu:ifu2.ra'),
	('SRIFU2CoordsDecDMS', 'Ghost', 'String', 'ghost:GhostCassegrainUnitCC.status.ifu2.std.dec', 'ghost:cc:cu:ifu2.dec'),
	('HRIFU1CoordsRADeg', 'Ghost', 'Double', 'ghost:GhostCassegrainUnitCC.status.ifu1.hi.fpx', 'ghost:cc:cu:ifu1.fpx'),
	('HRIFU1CoordsDecDeg', 'Ghost', 'Double', 'ghost:GhostCassegrainUnitCC.status.ifu1.hi.fpy', 'ghost:cc:cu:ifu1.fpy'),
	('HRIFU1CoordsRAHMS', 'Ghost', 'String', 'ghost:GhostCassegrainUnitCC.status.ifu1.hi.ra', 'ghost:cc:cu:ifu1.ra'),
	('HRIFU1CoordsDecDMS', 'Ghost', 'String', 'ghost:GhostCassegrainUnitCC.status.ifu1.hi.dec', 'ghost:cc:cu:ifu1.dec'),
	('HRIFU2CoordsRADeg', 'Ghost', 'Double', 'ghost:GhostCassegrainUnitCC.status.ifu2.hi.fpx', 'ghost:cc:cu:ifu2.fpx'),
	('HRIFU2CoordsDecDeg', 'Ghost', 'Double', 'ghost:GhostCassegrainUnitCC.status.ifu2.hi.fpy', 'ghost:cc:cu:ifu2.fpy'),
	('HRIFU2CoordsRAHMS', 'Ghost', 'String', 'ghost:GhostCassegrainUnitCC.status.ifu2.hi.ra', 'ghost:cc:cu:ifu2.ra'),
	('HRIFU2CoordsDecDMS', 'Ghost', 'String', 'ghost:GhostCassegrainUnitCC.status.ifu2.hi.dec', 'ghost:cc:cu:ifu2.dec');

