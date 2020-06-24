record PF_Bus_9
extends SMIBPS_IdControl.PF_Data.Bus_Template(

// Bus: 'B1' (PV bus)
V1 = 1.0000000,
A1 = 24.9876440,

// Bus: 'B2' (PQ bus)
V2 = 0.9792973,
A2 = 17.0639648,

// Bus: 'B3' (slack bus)
V3 = 1.0000000,
A3 = 0.0000000,

// Bus: 'B4' (PQ bus)
V4 = 0.9786664,
A4 = 7.9838613

);
end PF_Bus_9;