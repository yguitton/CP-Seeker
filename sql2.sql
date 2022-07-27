UPDATE chemical_ion SET chemical_type = REPLACE(chemical_type, 'CPs','PCAs');
UPDATE chemical_ion SET chemical_type = REPLACE(chemical_type, 'COs','PCOs');
UPDATE chemical_ion SET chemical_type = REPLACE(chemical_type, 'CdiOs','PCdiOs');
UPDATE chemical_ion SET chemical_type = REPLACE(chemical_type, 'OH-CPs','OH-PCAs');
UPDATE chemical_ion SET chemical_type = REPLACE(chemical_type, 'COOH-CPs','COOH-PCAs');
UPDATE chemical_ion SET chemical_type = REPLACE(chemical_type, 'oxo-CPs','oxo-PCAs');
UPDATE chemical_ion SET chemical_type = REPLACE(chemical_type, 'BPs','PBAs');
UPDATE chemical_ion SET chemical_type = REPLACE(chemical_type, 'XPs','PXAs');

UPDATE chemical SET chemical_family = REPLACE(chemical_family, 'Mixed paraffins','standard') WHERE chemical_type LIKE 'standard';

UPDATE chemical_ion SET adduct = REPLACE(adduct, 'HAc','Ac') WHERE adduct LIKE '%HAc%';