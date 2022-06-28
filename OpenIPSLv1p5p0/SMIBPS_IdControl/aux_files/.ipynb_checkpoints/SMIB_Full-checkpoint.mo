within ;
model SMIB_Full

  OpenIPSL.Electrical.Branches.PSAT.TwoWindingTransformer transformer(
    Sn=2220,
    x=0.15,
    r=0,
    V_b=400,
    Vn=400) annotation (Placement(transformation(extent={{-32,6},{-12,26}})));
  OpenIPSL.Electrical.Branches.PwLine line_1(
    R=0,
    G=0,
    B=0,
    X=0.5) annotation (Placement(transformation(extent={{38,30},{56,42}})));
  OpenIPSL.Electrical.Buses.InfiniteBus infinite_bus(angle_0=0, V_0=
        0.900810000000000) annotation (Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=0,
        origin={138,16})));
  OpenIPSL.Electrical.Branches.PwLine line_2(
    t1=0.57,
    R=0,
    G=0,
    B=0,
    X=0.93/2,
    opening=1)
    annotation (Placement(transformation(extent={{24,-12},{42,0}})));
  OpenIPSL.Electrical.Branches.PwLine line_3(
    R=0,
    G=0,
    B=0,
    X=0.93/2,
    opening=1)
    annotation (Placement(transformation(extent={{56,-12},{74,0}})));
  OpenIPSL.Electrical.Buses.BusExt busExt(V_b=400, np=1)
    annotation (Placement(transformation(extent={{-52,6},{-50,26}})));
  OpenIPSL.Electrical.Buses.BusExt B2(
    np=2,
    V_b=400,
    nn=1) annotation (Placement(transformation(extent={{0,6},{2,26}})));
  OpenIPSL.Electrical.Buses.BusExt B4(
    nn=2,
    V_b=400,
    np=1) annotation (Placement(transformation(extent={{48,-12},{50,0}})));
  OpenIPSL.Electrical.Buses.BusExt B3(
    V_b=400,
    np=1,
    nn=2) annotation (Placement(transformation(extent={{88,6},{90,26}})));
  OpenIPSL.Electrical.Events.PwFault fault(
    R=0,
    t1=0.5,
    t2=0.57,
    X=1e-5) annotation (Placement(transformation(extent={{-6,-6},{6,6}},
        rotation=270,
        origin={8,-20})));
  OpenIPSL.Electrical.Loads.PSSE.Load_ExtInput load_ExtInput
    annotation (Placement(transformation(extent={{40,-34},{52,-22}})));
  SMIBPS_IdControl.BaseModelsPartial.BasePlants.Generator G1(
    V_0=1,
    P_0=0.899999999997135*S_b,
    Q_0=0.436002238696658*S_b,
    angle_0=0.494677176989155*180/pi)
    annotation (Placement(transformation(extent={{-92,6},{-72,26}})));
equation
  connect(line_2.p,line_1. p) annotation (Line(points={{24.9,-6},{18,-6},{18,36},
          {38.9,36}},    color={0,0,255}));
  connect(busExt.p[1], transformer.p)
    annotation (Line(points={{-50,16},{-33,16}}, color={0,0,255}));
  connect(B2.n[1], transformer.n)
    annotation (Line(points={{0,16},{-11,16}}, color={0,0,255}));
  connect(B2.p[1], line_1.p) annotation (Line(points={{2,13},{18,13},{18,36},{
          38.9,36}}, color={0,0,255}));
  connect(B4.n[1], line_2.n) annotation (Line(points={{48,-7.8},{44,-7.8},{44,
          -6},{41.1,-6}}, color={0,0,255}));
  connect(B4.p[1], line_3.p)
    annotation (Line(points={{50,-6},{56.9,-6}}, color={0,0,255}));
  connect(B3.p[1], infinite_bus.p)
    annotation (Line(points={{90,16},{128,16}}, color={0,0,255}));
  connect(line_3.n, B3.n[1]) annotation (Line(points={{73.1,-6},{80,-6},{80,13},
          {88,13}}, color={0,0,255}));
  connect(line_1.n, B3.n[2]) annotation (Line(points={{55.1,36},{80,36},{80,18},
          {88,18},{88,19}}, color={0,0,255}));
  connect(fault.p, B2.p[2])
    annotation (Line(points={{8,-13},{8,19},{2,19}}, color={0,0,255}));
  connect(load_ExtInput.p, B4.n[2])
    annotation (Line(points={{46,-22},{46,-4.2},{48,-4.2}}, color={0,0,255}));
  connect(G1.pwPin, B1.p)
    annotation (Line(points={{-71,16},{-52,16}},
                                               color={0,0,255}));
  annotation (
    uses(OpenIPSL(version="1.5.0"), SMIBPS_IdControl(version="1")),
    Diagram(coordinateSystem(extent={{-100,-60},{180,60}})),
    Icon(coordinateSystem(extent={{-100,-60},{180,60}})));
end SMIB_Full;
