record PF_Bus_8
extends SMIBPS_IdControl.PF_Data.Bus_Template(

// Bus: 'B1' (PV bus)
V1 = 1.0000000,
A1 = 24.9783286,

// Bus: 'B2' (PQ bus)
V2 = 0.9793044,
A2 = 17.0547074,

// Bus: 'B3' (slack bus)
V3 = 1.0000000,
A3 = 0.0000000,

// Bus: 'B4' (PQ bus)
V4 = 0.9786800,
A4 = 7.9663993

);
end PF_Bus_8;