within SMIBPS_IdControl.PF_Data;
record PowerFlow_Data
extends Modelica.Icons.Record;

replaceable record Bus = SMIBPS_IdControl.PF_Data.Bus_Template constrainedby
    PF_Data.Bus_Template
annotation(choicesAllMatching);
Bus bus;

replaceable record Loads = SMIBPS_IdControl.PF_Data.Loads_Template constrainedby
    PF_Data.Loads_Template
annotation(choicesAllMatching);
Loads loads;

replaceable record Trafos = SMIBPS_IdControl.PF_Data.Trafos_Template constrainedby
    PF_Data.Trafos_Template
annotation(choicesAllMatching);
Trafos trafos;

replaceable record Machines = SMIBPS_IdControl.PF_Data.Machines_Template constrainedby
    PF_Data.Machines_Template
annotation(choicesAllMatching);
Machines machines;

end PowerFlow_Data;
