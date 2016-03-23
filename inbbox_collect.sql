SELECT ST_AsText(ST_Collect(f.geom))
FROM (
	SELECT ST_AsText(wkb_geometry) As geom
	FROM fires
	WHERE wkb_geometry && ST_SetSRID(ST_MakeBox2D(ST_Point(6400000, 1950000),ST_Point(6500000 ,2050000)),2229) AND
	fires.time >= '1990-01-01' AND fires.time < '2000-01-01') as f;
	