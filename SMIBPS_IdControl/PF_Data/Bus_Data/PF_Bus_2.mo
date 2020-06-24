record PF_Bus_2
extends SMIBPS_IdControl.PF_Data.Bus_Template(

// Bus: 'B1' (PV bus)
V1 = 1.0000000,
A1 = 24.8746632,

// Bus: 'B2' (PQ bus)
V2 = 0.9791363,
A2 = 16.9496729,

// Bus: 'B3' (slack bus)
V3 = 1.0000000,
A3 = 0.0000000,

// Bus: 'B4' (PQ bus)
V4 = 0.9776339,
A4 = 7.7504207

);
end PF_Bus_2;