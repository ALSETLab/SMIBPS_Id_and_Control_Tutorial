within SMIBPS_IdControl;
package Analysis
  extends Modelica.Icons.ExamplesPackage;

  package Simulation
    extends Modelica.Icons.ExamplesPackage;
    model SMIB
      extends Modelica.Icons.Example;
      extends BaseModelsPartial.BaseNetwork.SMIB_Partial(
        powerFlow_Data(
          redeclare record Bus = PF_Data.Bus_Data.PF_Bus_5,
          redeclare record Loads = PF_Data.Loads_Data.PF_Loads_5,
          redeclare record Trafos = PF_Data.Trafos_Data.PF_Trafos_5,
          redeclare record Machines = PF_Data.Machines_Data.PF_Machines_5),
        transformer(kT=1),
        load_ExtInput(
          d_P=0,
          t1=0,
          d_t=0),
        fault(t2=0.5 + 5/60));
      import Modelica.Constants.pi;
      BaseModelsPartial.BasePlants.Generator G1(
        V_0=powerFlow_Data.bus.V1,
        P_0=powerFlow_Data.machines.PG1,
        Q_0=powerFlow_Data.machines.QG1,
        angle_0=powerFlow_Data.bus.A1)
        annotation (Placement(transformation(extent={{-122,-10},{-102,10}})));
      Modelica.Blocks.Sources.Constant const(k=0)
        annotation (Placement(transformation(extent={{-2,-72},{6,-64}})));
    protected
      parameter Real S_b=SysData.S_b;
    equation
      connect(const.y, load_ExtInput.u) annotation (Line(points={{6.4,-68},{10,
              -68},{10,-66.7},{17.14,-66.7}}, color={0,0,127}));
      connect(G1.pwPin, B1.p)
        annotation (Line(points={{-101,0},{-80,0}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(extent={{-140,-100},{120,100}},
              preserveAspectRatio=false), graphics={Text(
              extent={{-10,70},{114,46}},
              lineColor={0,0,0},
              lineThickness=1,
              fillPattern=FillPattern.Solid,
              fontSize=15,
              textStyle={TextStyle.Bold},
              textString="(Constant Efd)")}),
        Icon(coordinateSystem(extent={{-140,-100},{120,100}})),
        experiment(
          StopTime=10,
          Interval=0.0001,
          Tolerance=1e-006,
          __Dymola_fixedstepsize=0.0001,
          __Dymola_Algorithm="Rkfix2"),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
    end SMIB;

    model SMIB_AVR
        extends Modelica.Icons.Example;
      extends BaseModelsPartial.BaseNetwork.SMIB_Partial(powerFlow_Data(
          redeclare record Bus = PF_Data.Bus_Data.PF_Bus_3,
          redeclare record Loads = PF_Data.Loads_Data.PF_Loads_3,
          redeclare record Trafos = PF_Data.Trafos_Data.PF_Trafos_3,
          redeclare record Machines = PF_Data.Machines_Data.PF_Machines_3),
          load_ExtInput(
          d_P=0,
          t1=0,
          d_t=0),
        fault(t2=0.5 + 5/60));
      import Modelica.Constants.pi;
      BaseModelsPartial.BasePlants.Generator_AVR G1(
        V_0=powerFlow_Data.bus.V1,
        P_0=powerFlow_Data.machines.PG1,
        Q_0=powerFlow_Data.machines.QG1,
        angle_0=powerFlow_Data.bus.A1)
        annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
      Modelica.Blocks.Sources.Constant const(k=0)
        annotation (Placement(transformation(extent={{0,-72},{8,-64}})));
    protected
      parameter Real S_b=SysData.S_b;
    equation
      connect(const.y, load_ExtInput.u) annotation (Line(points={{8.4,-68},{10,
              -68},{10,-66.7},{17.14,-66.7}}, color={0,0,127}));
      connect(G1.pwPin, B1.p)
        annotation (Line(points={{-99,0},{-80,0}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(extent={{-140,-100},{120,100}},
              preserveAspectRatio=false), graphics={Text(
              extent={{-110,70},{110,50}},
              lineColor={0,0,0},
              lineThickness=1,
              fillPattern=FillPattern.Solid,
              fontSize=15,
              textStyle={TextStyle.Bold},
              textString="(AVR)")}),
        Icon(coordinateSystem(extent={{-140,-100},{120,100}})),
        experiment(
          StopTime=10,
          Interval=0.0001,
          Tolerance=1e-006,
          __Dymola_fixedstepsize=0.0001,
          __Dymola_Algorithm="Rkfix2"),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
    end SMIB_AVR;

    model SMIB_AVR_PSS
        extends Modelica.Icons.Example;
      extends BaseModelsPartial.BaseNetwork.SMIB_Partial(powerFlow_Data(
          redeclare record Bus = PF_Data.Bus_Data.PF_Bus_8,
          redeclare record Loads = PF_Data.Loads_Data.PF_Loads_8,
          redeclare record Trafos = PF_Data.Trafos_Data.PF_Trafos_8,
          redeclare record Machines = PF_Data.Machines_Data.PF_Machines_8),
          load_ExtInput(
          d_P=0,
          t1=0,
          d_t=0),
        fault(t2=0.5 + 5/60));
      import Modelica.Constants.pi;
      BaseModelsPartial.BasePlants.Generator_AVR_PSS G1(
        V_0=powerFlow_Data.bus.V1,
        P_0=powerFlow_Data.machines.PG1,
        Q_0=powerFlow_Data.machines.QG1,
        angle_0=powerFlow_Data.bus.A1)
        annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
      Modelica.Blocks.Sources.Constant const(k=0)
        annotation (Placement(transformation(extent={{0,-70},{8,-62}})));
    protected
      parameter Real S_b=SysData.S_b;
    equation
      connect(const.y, load_ExtInput.u) annotation (Line(points={{8.4,-66},{10,
              -66},{10,-66.7},{17.14,-66.7}}, color={0,0,127}));
      connect(G1.pwPin, B1.p)
        annotation (Line(points={{-99,0},{-80,0}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(extent={{-140,-100},{120,100}},
              preserveAspectRatio=false), graphics={Text(
              extent={{-112,64},{108,44}},
              lineColor={0,0,0},
              lineThickness=1,
              fillPattern=FillPattern.Solid,
              fontSize=15,
              textStyle={TextStyle.Bold},
              textString="(AVR + PSS)")}),
        Icon(coordinateSystem(extent={{-140,-100},{120,100}})),
        experiment(
          StopTime=10,
          Interval=0.0001,
          Tolerance=1e-006,
          __Dymola_fixedstepsize=0.0001,
          __Dymola_Algorithm="Rkfix2"),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
    end SMIB_AVR_PSS;

    function RunAndCompare
      "Runs the three simulation cases and compares their variables"
      // INPUTS TO THE FUNCTION
      // Declare reconfigurable simulation parameters
      input Modelica.SIunits.Time tsim = 10 "Simulation time";
      input Integer numberOfIntervalsin=10000 "No. of intervals";
      //input String methodin = "Rkfix4" "Solver";
      input String methodin = "Dassl" "Solver";
      input Real fixedstepsizein= 1e-3 "Time step - needed only for fixed time step solvers";
      //
      // MODELS TO SIMULATE
      input String modela="SMIBPS_IdControl.Analysis.Simulation.SMIB";
      input String modelb="SMIBPS_IdControl.Analysis.Simulation.SMIB_AVR";
      input String modelc="SMIBPS_IdControl.Analysis.Simulation.SMIB_AVR_PSS";

    algorithm
      simulateModel(
        modela,
        stopTime=tsim,
        numberOfIntervals=numberOfIntervalsin,
        method = methodin,
        fixedstepsize=fixedstepsizein,
        resultFile="res_casea");
      simulateModel(
        modelb,
        stopTime=tsim,
        numberOfIntervals=numberOfIntervalsin,
        method = methodin,
        fixedstepsize=fixedstepsizein,
        resultFile="res_caseb");
    simulateModel(
        modelc,
        stopTime=tsim,
        numberOfIntervals=numberOfIntervalsin,
        method = methodin,
        fixedstepsize=fixedstepsizein,
        resultFile="res_casec");

    removePlots(true);
    Advanced.FilesToKeep :=10;
    createPlot(id=1, position={15, 15, 678, 703}, y={"B1.V"}, range={0.0, 10.0, 0.4, 1.4}, grid=true, filename="res_casea.mat", colors={{28,108,200}}, displayUnits={"1"});
    createPlot(id=1, position={15, 15, 678, 703}, y={"G1.machine.P"}, range={0.0, 10.0, -1.5, 2.0}, grid=true, subPlot=102, colors={{28,108,200}}, displayUnits={"1"});
    createPlot(id=1, position={15, 15, 678, 703}, y={"G1.machine.Q"}, range={0.0, 10.0, -0.5, 2.0}, grid=true, subPlot=103, colors={{28,108,200}}, displayUnits={"1"});
    createPlot(id=1, position={15, 15, 678, 703}, y={"B1.V"}, range={0.0, 10.0, 0.4, 1.4}, erase=false, grid=true, filename="res_caseb.mat", colors={{238,46,47}}, displayUnits={"1"});
    createPlot(id=1, position={15, 15, 678, 703}, y={"G1.machine.P"}, range={0.0, 10.0, -1.5, 2.0}, erase=false, grid=true, subPlot=102, colors={{238,46,47}}, displayUnits={"1"});
    createPlot(id=1, position={15, 15, 678, 703}, y={"G1.machine.Q"}, range={0.0, 10.0, -0.5, 2.0}, erase=false, grid=true, subPlot=103, colors={{238,46,47}}, displayUnits={"1"});
    createPlot(id=1, position={15, 15, 678, 703}, y={"B1.V"}, range={0.0, 10.0, 0.4, 1.4}, erase=false, grid=true, filename="res_casec.mat", colors={{0,140,72}}, displayUnits={"1"});
    createPlot(id=1, position={15, 15, 678, 703}, y={"G1.machine.P"}, range={0.0, 10.0, -1.5, 2.0}, erase=false, grid=true, subPlot=102, colors={{0,140,72}}, displayUnits={"1"});
    createPlot(id=1, position={15, 15, 678, 703}, y={"G1.machine.Q"}, range={0.0, 10.0, -0.5, 2.0}, erase=false, grid=true, subPlot=103, colors={{0,140,72}}, displayUnits={"1"});


    end RunAndCompare;
  end Simulation;

  package LinearAnalysis
    extends Modelica.Icons.ExamplesPackage;
    package Interfaces
      model SMIB_GEN_wInput
        extends BaseModelsPartial.BaseNetwork.SMIB_BaseWithPF(powerFlow_Data(
            redeclare record Bus = PF_Data.Bus_Data.PF_Bus_5,
            redeclare record Loads = PF_Data.Loads_Data.PF_Loads_5,
            redeclare record Trafos = PF_Data.Trafos_Data.PF_Trafos_5,
            redeclare record Machines = PF_Data.Machines_Data.PF_Machines_5));
          extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;

        import Modelica.Constants.pi;
        BaseModelsPartial.BasePlants.Generator_wInputs G1(
          V_0=powerFlow_Data.bus.V1,
          P_0=powerFlow_Data.machines.PG1,
          Q_0=powerFlow_Data.machines.QG1,
          angle_0=powerFlow_Data.bus.A1)
          annotation (Placement(transformation(extent={{-120,-12},{-96,12}})));
        Modelica.Blocks.Interfaces.RealInput uEfd
          annotation (Placement(transformation(extent={{-180,20},{-140,60}})));
        Modelica.Blocks.Interfaces.RealInput uPm
          annotation (Placement(transformation(extent={{-180,-60},{-140,-20}})));
        Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
              transformation(extent={{-180,-120},{-140,-80}})));
      protected
        parameter Real S_b=SysData.S_b;
      equation
        w = G1.machine.w;
        delta = G1.machine.delta;
        Vt = G1.machine.v;
        P = G1.machine.P;
        Q = G1.machine.Q;
        connect(G1.pm, uPm) annotation (Line(points={{-122.4,-7.2},{-136,-7.2},{-136,-40},
                {-160,-40}}, color={0,0,127}));
        connect(G1.efd, uEfd) annotation (Line(points={{-122.4,7.2},{-136,7.2},{-136,40},
                {-160,40}}, color={0,0,127}));
        connect(G1.pwPin, B1.p) annotation (Line(points={{-94.8,0},{-88,0},{-88,0},{-80,
                0}}, color={0,0,255}));
        connect(uPload, load_ExtInput.u) annotation (Line(points={{-160,-100},{
                -72,-100},{-72,-66.7},{17.14,-66.7}}, color={0,0,127}));
        annotation (
          Diagram(coordinateSystem(extent={{-140,-140},{140,140}}),
                                            graphics={Text(
                extent={{-108,140},{112,120}},
                lineColor={0,0,0},
                lineThickness=1,
                fontSize=15,
                textStyle={TextStyle.Bold},
                textString="(Generator + input for Bode plots)")}),
          Icon(coordinateSystem(extent={{-140,-140},{140,140}}), graphics={
              Rectangle(extent={{-140,140},{140,-140}}, lineColor={28,108,200}),
              Text(
                extent={{-140,60},{140,-80}},
                lineColor={28,108,200},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="Nonlinear
Model
Generator
"),           Text(
                extent={{-140,-80},{140,-220}},
                lineColor={238,46,47},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="%name
")}),     experiment(
            StopTime=10,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
      end SMIB_GEN_wInput;
      extends Modelica.Icons.InterfacesPackage;
      model SMIB_AVR_wInput
        extends BaseModelsPartial.BaseNetwork.SMIB_BaseWithPF(powerFlow_Data(
            redeclare record Bus = PF_Data.Bus_Data.PF_Bus_5,
            redeclare record Loads = PF_Data.Loads_Data.PF_Loads_5,
            redeclare record Trafos = PF_Data.Trafos_Data.PF_Trafos_5,
            redeclare record Machines = PF_Data.Machines_Data.PF_Machines_5));
          extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;

        import Modelica.Constants.pi;
        BaseModelsPartial.BasePlants.Generator_AVR_wInputs G1(
          V_0=powerFlow_Data.bus.V1,
          P_0=powerFlow_Data.machines.PG1,
          Q_0=powerFlow_Data.machines.QG1,
          angle_0=powerFlow_Data.bus.A1)
          annotation (Placement(transformation(extent={{-116,-10},{-92,14}})));
        Modelica.Blocks.Interfaces.RealInput uVref
          annotation (Placement(transformation(extent={{-180,60},{-140,100}}),
              iconTransformation(extent={{-180,60},{-140,100}})));
        Modelica.Blocks.Interfaces.RealInput uPm
          annotation (Placement(transformation(extent={{-180,-20},{-140,20}}),
              iconTransformation(extent={{-180,-20},{-140,20}})));
        Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
              transformation(extent={{-180,-100},{-140,-60}}),
              iconTransformation(extent={{-180,-100},{-140,-60}})));
      protected
        parameter Real S_b=SysData.S_b;
      equation
        w = G1.machine.w;
        delta = G1.machine.delta;
        Vt = G1.machine.v;
        P = G1.machine.P;
        Q = G1.machine.Q;
        connect(uVref, G1.uVs) annotation (Line(points={{-160,80},{-134,80},{
                -134,9.2},{-118.64,9.2}},
                                color={0,0,127}));
        connect(G1.pm, uPm) annotation (Line(points={{-118.4,-5.2},{-134,-5.2},
                {-134,0},{-160,0}},
                             color={0,0,127}));
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-90.8,2},{-86,2},{-86,0},{-80,0}},
                                                       color={0,0,255}));
        connect(uPload, load_ExtInput.u) annotation (Line(points={{-160,-80},{
                -72,-80},{-72,-66.7},{17.14,-66.7}},  color={0,0,127}));
        annotation (
          Diagram(coordinateSystem(extent={{-140,-140},{140,140}}),
                                            graphics={Text(
                extent={{-138,138},{138,118}},
                lineColor={0,0,0},
                lineThickness=1,
                fontSize=15,
                textStyle={TextStyle.Bold},
                textString="(AVR
 + input for Bode plots)")}),
          Icon(coordinateSystem(extent={{-140,-140},{140,140}}), graphics={
              Rectangle(extent={{-140,140},{140,-140}}, lineColor={28,108,200}),
              Text(
                extent={{-140,60},{140,-80}},
                lineColor={28,108,200},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="Nonlinear
Model
with AVR
"),           Text(
                extent={{-140,-80},{140,-220}},
                lineColor={238,46,47},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="%name
")}),     experiment(
            StopTime=10,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
      end SMIB_AVR_wInput;

      model SMIB_AVR_PSS_wInput
        extends BaseModelsPartial.BaseNetwork.SMIB_BaseWithPF(powerFlow_Data(
            redeclare record Bus = PF_Data.Bus_Data.PF_Bus_5,
            redeclare record Loads = PF_Data.Loads_Data.PF_Loads_5,
            redeclare record Trafos = PF_Data.Trafos_Data.PF_Trafos_5,
            redeclare record Machines = PF_Data.Machines_Data.PF_Machines_5));
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        import Modelica.Constants.pi;
        BaseModelsPartial.BasePlants.Generator_AVR_PSS_wInputs G1(
          V_0=powerFlow_Data.bus.V1,
          P_0=powerFlow_Data.machines.PG1,
          Q_0=powerFlow_Data.machines.QG1,
          angle_0=powerFlow_Data.bus.A1)
          annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
        Modelica.Blocks.Interfaces.RealInput uPSS
          annotation (Placement(transformation(extent={{-180,60},{-140,100}}),
              iconTransformation(extent={{-180,60},{-140,100}})));
        Modelica.Blocks.Interfaces.RealInput uPm
          annotation (Placement(transformation(extent={{-180,-20},{-140,20}}),
              iconTransformation(extent={{-180,-20},{-140,20}})));
        Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
              transformation(extent={{-180,-100},{-140,-60}}),
              iconTransformation(extent={{-180,-100},{-140,-60}})));
      protected
        parameter Real S_b=SysData.S_b;
      equation
        w = G1.machine.w;
        delta = G1.machine.delta;
        Vt = G1.machine.v;
        P = G1.machine.P;
        Q = G1.machine.Q;
        connect(G1.uPSS, uPSS) annotation (Line(points={{-122,6},{-128,6},{-128,
                80},{-160,80}},
                      color={0,0,127}));
        connect(uPm, G1.pm) annotation (Line(points={{-160,0},{-130,0},{-130,-6},
                {-122,-6}},color={0,0,127}));
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-99,0},{-80,0}}, color={0,0,255}));
        connect(load_ExtInput.u, uPload) annotation (Line(points={{17.14,-66.7},
                {-18,-66.7},{-18,-80},{-160,-80}},   color={0,0,127}));
        annotation (
          Diagram(coordinateSystem(extent={{-140,-140},{140,140}}),
                                            graphics={Text(
                extent={{-78,100},{142,80}},
                lineColor={0,0,0},
                lineThickness=1,
                fontSize=15,
                textStyle={TextStyle.Bold},
                textString="(AVR + PSS + input for Bode plots)")}),
          Icon(coordinateSystem(extent={{-140,-140},{140,140}}), graphics={
              Rectangle(extent={{-140,140},{140,-140}}, lineColor={28,108,200}),
              Text(
                extent={{-140,60},{140,-80}},
                lineColor={28,108,200},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="Nonlinear
Model
AVR+PSS

"),           Text(
                extent={{-140,-80},{140,-220}},
                lineColor={238,46,47},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="%name
")}),     experiment(
            StopTime=10,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
      end SMIB_AVR_PSS_wInput;

      model SMIB_AVR_PSS_wInput_wLineRmoval
        extends BaseModelsPartial.BaseNetwork.SMIB_Partial(powerFlow_Data(
            redeclare record Bus = PF_Data.Bus_Data.PF_Bus_5,
            redeclare record Loads = PF_Data.Loads_Data.PF_Loads_5,
            redeclare record Trafos = PF_Data.Trafos_Data.PF_Trafos_5,
            redeclare record Machines = PF_Data.Machines_Data.PF_Machines_5),
          line_1(X=3.25),
          line_2(t1=Modelica.Constants.inf),
          fault(t1=Modelica.Constants.inf, t2=Modelica.Constants.inf));
        extends SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        import Modelica.Constants.pi;
        BaseModelsPartial.BasePlants.Generator_AVR_PSS_wInputs G1(
          V_0=powerFlow_Data.bus.V1,
          P_0=powerFlow_Data.machines.PG1,
          Q_0=powerFlow_Data.machines.QG1,
          angle_0=powerFlow_Data.bus.A1)
          annotation (Placement(transformation(extent={{-120,-8},{-100,12}})));
        Modelica.Blocks.Interfaces.RealInput uPSS
          annotation (Placement(transformation(extent={{-180,60},{-140,100}}),
              iconTransformation(extent={{-180,60},{-140,100}})));
        Modelica.Blocks.Interfaces.RealInput uPm
          annotation (Placement(transformation(extent={{-180,-20},{-140,20}})));
        Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
              transformation(extent={{-180,-100},{-140,-60}}),
              iconTransformation(extent={{-180,-100},{-140,-60}})));

        OpenIPSL.Electrical.Branches.PwLine line_4(
          R=0,
          G=0,
          B=0,
          X=3.25/5.5,
          t1=t1,
          t2=t2,
          opening=opening)
                 annotation (Placement(transformation(extent={{22,2},{40,14}})));
        parameter Modelica.SIunits.Time t1=Modelica.Constants.inf "Time of line removal"
          annotation (Dialog(group="Line Removal Parameters"));
        parameter Modelica.SIunits.Time t2=Modelica.Constants.inf
          "Line re-insertion time"     annotation (Dialog(group="Line Removal Parameters"));
        parameter Integer opening=1
          "Type of opening (1: removes both ends at same time, 2: removes sending end, 3: removes receiving end)"     annotation (Dialog(group="Line Removal Parameters"));
      protected
        parameter Real S_b=SysData.S_b;
      equation
        w = G1.machine.w;
        delta = G1.machine.delta;
        Vt = G1.machine.v;
        P = G1.machine.P;
        Q = G1.machine.Q;
        connect(load_ExtInput.u, uPload) annotation (Line(points={{17.14,-66.7},
                {-2,-66.7},{-2,-80},{-160,-80}},     color={0,0,127}));
        connect(G1.uPSS, uPSS) annotation (Line(points={{-122,8},{-128,8},{-128,
                80},{-160,80}},
                      color={0,0,127}));
        connect(uPm, G1.pm) annotation (Line(points={{-160,0},{-130,0},{-130,-4},
                {-122,-4}},color={0,0,127}));
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-99,2},{-90,2},{-90,0},{-80,0}},
                                                     color={0,0,255}));
        connect(line_4.n, line_1.n)
          annotation (Line(points={{39.1,8},{39.1,20}}, color={0,0,255}));
        connect(line_4.p, line_1.p)
          annotation (Line(points={{22.9,8},{22.9,20}}, color={0,0,255}));
        annotation (
          Diagram(coordinateSystem(extent={{-140,-140},{140,140}}), graphics={
                Rectangle(
                extent={{12,28},{50,0}},
                lineColor={238,46,47},
                fillColor={244,125,35},
                fillPattern=FillPattern.None,
                lineThickness=1)}),
          Icon(coordinateSystem(extent={{-140,-140},{140,140}}), graphics={
              Rectangle(extent={{-140,140},{140,-140}}, lineColor={28,108,200}),
              Text(
                extent={{-140,60},{140,-80}},
                lineColor={28,108,200},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="Nonlinear
Model
AVR+PSS

"),           Text(
                extent={{-140,-80},{140,-220}},
                lineColor={238,46,47},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="%name
")}),     experiment(
            StopTime=20,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
      end SMIB_AVR_PSS_wInput_wLineRmoval;

      model SMIB_AVR_PSS_wInput_wLineRmovaland4inputs
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterfaceBusVoltagesBranchPowers;
        extends BaseModelsPartial.BaseNetwork.SMIB_Partial(powerFlow_Data(
            redeclare record Bus = PF_Data.Bus_Data.PF_Bus_5,
            redeclare record Loads = PF_Data.Loads_Data.PF_Loads_5,
            redeclare record Trafos = PF_Data.Trafos_Data.PF_Trafos_5,
            redeclare record Machines = PF_Data.Machines_Data.PF_Machines_5),
          line_1(X=3.25),
          line_2(t1=Modelica.Constants.inf),
          fault(t1=Modelica.Constants.inf, t2=Modelica.Constants.inf));
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterfaceBig;
        import Modelica.Constants.pi;
        BaseModelsPartial.BasePlants.Generator_AVR_PSS_w3Inputs G1(
          V_0=powerFlow_Data.bus.V1,
          P_0=powerFlow_Data.machines.PG1,
          Q_0=powerFlow_Data.machines.QG1,
          angle_0=powerFlow_Data.bus.A1)
          annotation (Placement(transformation(extent={{-120,-8},{-100,12}})));
        Modelica.Blocks.Interfaces.RealInput uPSS
          annotation (Placement(transformation(extent={{-180,60},{-140,100}}),
              iconTransformation(extent={{-180,60},{-140,100}})));
        Modelica.Blocks.Interfaces.RealInput uPm
          annotation (Placement(transformation(extent={{-180,-20},{-140,20}})));
        Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
              transformation(extent={{-180,-80},{-140,-40}}),
              iconTransformation(extent={{-180,-80},{-140,-40}})));

        OpenIPSL.Electrical.Branches.PwLine line_4(
          R=0,
          G=0,
          B=0,
          X=3.25/5.5,
          t1=t1,
          t2=t2,
          opening=opening)
                 annotation (Placement(transformation(extent={{22,2},{40,14}})));
        parameter Modelica.SIunits.Time t1=Modelica.Constants.inf "Time of line removal"
          annotation (Dialog(group="Line Removal Parameters"));
        parameter Modelica.SIunits.Time t2=Modelica.Constants.inf
          "Line re-insertion time"     annotation (Dialog(group="Line Removal Parameters"));
        parameter Integer opening=1
          "Type of opening (1: removes both ends at same time, 2: removes sending end, 3: removes receiving end)"     annotation (Dialog(group="Line Removal Parameters"));
        Modelica.Blocks.Interfaces.RealInput uvsAVR
          annotation (Placement(transformation(extent={{-180,-140},{-140,-100}})));
      protected
        parameter Real S_b=SysData.S_b;
      equation
        w = G1.machine.w;
        delta = G1.machine.delta;
        Vt = G1.machine.v;
        P = G1.machine.P;
        Q = G1.machine.Q;
        // Assignment of outputs for the network voltages and powers
        Bvm1 = B1.V;
        Bva1 = B1.angle;
        Bvm2 = B2.V;
        Bva2 = B2.angle;
        Bvm3 = B3.V;
        Bva3 = B3.angle;
        Bvm4 = B4.V;
        Bva4 = B4.angle;
        Pline1 = line_1.P12;
        Qline1 = line_1.Q12;
        Pline2 = line_2.P12;
        Qline2 = line_2.Q12;
        Pline3 = line_3.P12;
        Qline3 = line_3.Q12;
        Pline4 = line_4.P12;
        Qline4 = line_4.Q12;
        Bvadiff1to2 = B1.angle - B2.angle;
        Bvadiff1to3 = B1.angle - B3.angle;
        Bvadiff1to4 = B1.angle - B4.angle;
        //
        connect(load_ExtInput.u, uPload) annotation (Line(points={{17.14,-66.7},{-2,-66.7},
                {-2,-60},{-160,-60}},                color={0,0,127}));
        connect(G1.uPSS, uPSS) annotation (Line(points={{-122,8},{-128,8},{-128,
                80},{-160,80}},
                      color={0,0,127}));
        connect(uPm, G1.upm) annotation (Line(points={{-160,0},{-130,0},{-130,-4},{-122,
                -4}}, color={0,0,127}));
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-99,2},{-90,2},{-90,0},{-80,0}},
                                                     color={0,0,255}));
        connect(line_4.n, line_1.n)
          annotation (Line(points={{39.1,8},{39.1,20}}, color={0,0,255}));
        connect(line_4.p, line_1.p)
          annotation (Line(points={{22.9,8},{22.9,20}}, color={0,0,255}));
        connect(G1.uvs, uvsAVR) annotation (Line(points={{-110,-10},{-106,-10},{-106,-120},
                {-160,-120}}, color={0,0,127}));
        annotation (
          Diagram(coordinateSystem(extent={{-140,-240},{240,140}}), graphics={
                Rectangle(
                extent={{12,28},{50,0}},
                lineColor={238,46,47},
                fillColor={244,125,35},
                fillPattern=FillPattern.None,
                lineThickness=1)}),
          Icon(coordinateSystem(extent={{-140,-240},{240,140}}), graphics={
              Rectangle(extent={{-140,140},{240,-140}}, lineColor={28,108,200}),
              Text(
                extent={{-140,40},{240,-60}},
                lineColor={28,108,200},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="Nonlinear
Model
AVR+PSS

"),           Text(
                extent={{-86,134},{194,94}},
                lineColor={238,46,47},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="%name
")}),     experiment(
            StopTime=20,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
      end SMIB_AVR_PSS_wInput_wLineRmovaland4inputs;

      partial model OutputsInterface
      public
        Modelica.Blocks.Interfaces.RealOutput Vt
          annotation (Placement(transformation(extent={{140,70},{160,90}}),
              iconTransformation(extent={{140,70},{160,90}})));
        Modelica.Blocks.Interfaces.RealOutput Q
          annotation (Placement(transformation(extent={{140,-10},{160,10}}),
              iconTransformation(extent={{140,-10},{160,10}})));
        Modelica.Blocks.Interfaces.RealOutput P
          annotation (Placement(transformation(extent={{140,30},{160,50}}),
              iconTransformation(extent={{140,30},{160,50}})));
        Modelica.Blocks.Interfaces.RealOutput w
          annotation (Placement(transformation(extent={{140,-50},{160,-30}}),
              iconTransformation(extent={{140,-50},{160,-30}})));
        Modelica.Blocks.Interfaces.RealOutput delta
          annotation (Placement(transformation(extent={{140,-90},{160,-70}}),
              iconTransformation(extent={{140,-90},{160,-70}})));
        annotation (
          experiment(
            StopTime=10,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"),Diagram(coordinateSystem(extent={{-140,-140},{140,140}})),
          Icon(coordinateSystem(extent={{-140,-140},{140,140}})));
      end OutputsInterface;

      partial model OutputsInterfaceBig
      public
        Modelica.Blocks.Interfaces.RealOutput Vt
          annotation (Placement(transformation(extent={{240,70},{260,90}}),
              iconTransformation(extent={{240,70},{260,90}})));
        Modelica.Blocks.Interfaces.RealOutput Q
          annotation (Placement(transformation(extent={{240,-10},{260,10}}),
              iconTransformation(extent={{240,-10},{260,10}})));
        Modelica.Blocks.Interfaces.RealOutput P
          annotation (Placement(transformation(extent={{240,30},{260,50}}),
              iconTransformation(extent={{240,30},{260,50}})));
        Modelica.Blocks.Interfaces.RealOutput w
          annotation (Placement(transformation(extent={{240,-50},{260,-30}}),
              iconTransformation(extent={{240,-50},{260,-30}})));
        Modelica.Blocks.Interfaces.RealOutput delta
          annotation (Placement(transformation(extent={{240,-90},{260,-70}}),
              iconTransformation(extent={{240,-90},{260,-70}})));
        annotation (
          experiment(
            StopTime=10,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"),Diagram(coordinateSystem(extent={{-140,-140},{240,140}})),
          Icon(coordinateSystem(extent={{-140,-140},{240,140}})));
      end OutputsInterfaceBig;

      partial model OutputsInterfaceBusVoltagesBranchPowers
        Modelica.Blocks.Interfaces.RealOutput Pline1 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-100,-210}),
                                 iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={30,-150})));
        Modelica.Blocks.Interfaces.RealOutput Qline1 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-80,-210}),iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={110,-150})));
        Modelica.Blocks.Interfaces.RealOutput Pline2 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={20,-210}),iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={50,-150})));
        Modelica.Blocks.Interfaces.RealOutput Qline2 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={40,-210}),iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={130,-150})));
        Modelica.Blocks.Interfaces.RealOutput Pline3 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={80,-210}), iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={70,-150})));
        Modelica.Blocks.Interfaces.RealOutput Qline3 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={100,-210}),iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={150,-150})));
        Modelica.Blocks.Interfaces.RealOutput Pline4 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-40,-210}),
                                iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={90,-150})));
        Modelica.Blocks.Interfaces.RealOutput Qline4 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-20,-210}),
                                iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={170,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bvm1 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-100,-170}),
                                iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-130,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bva1 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-80,-170}),
                                iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-50,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bvm2 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-40,-170}),
                               iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-110,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bva2 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-20,-170}),
                               iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-30,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bvm4 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={80,-170}),iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-70,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bva4 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={100,-170}),
                                iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={10,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bvadiff1to2 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={130,-170}), iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={190,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bvadiff1to3 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={152,-170}), iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={210,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bvadiff1to4 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={172,-170}), iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={230,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bvm3 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={22,-170}),
                               iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-90,-150})));
        Modelica.Blocks.Interfaces.RealOutput Bva3 annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={42,-170}),
                               iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-10,-150})));
      equation
      //   Bvm1 = B1.V;
      //   Bva1 = B1.angle;
      //   Bvm2 = B2.V;
      //   Bva2 = B2.angle;
      //   Bvm3 = B3.V;
      //   Bva3 = B3.angle;
      //   Bvm4 = B4.V;
      //   Bva4 = B4.angle;
      //   Pline1 = line_1.P12;
      //   Qline1 = line_1.Q12;
      //   Pline2 = line_2.P12;
      //   Qline2 = line_2.Q12;
      //   Pline3 = line_3.P12;
      //   Qline3 = line_3.Q12;
      //   Pline4 = line_4.P12;
      //   Qline4 = line_4.Q12;
        annotation (
          experiment(
            StopTime=10,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"),Diagram(coordinateSystem(extent={{-140,-220},{220,140}}),
                                                                  graphics={Rectangle(
                extent={{-120,-190},{0,-220}},
                lineColor={255,0,0},
                lineThickness=1,
                fillColor={255,170,213},
                fillPattern=FillPattern.Solid), Rectangle(
                extent={{-130,-140},{200,-220}},
                lineColor={28,108,200},
                lineThickness=1),
              Text(
                extent={{-130,-140},{130,-160}},
                lineColor={255,0,0},
                lineThickness=1,
                fillColor={255,170,213},
                fillPattern=FillPattern.None,
                textString="Assignment of Outputs is Done in the Text Layer
")}),     Icon(coordinateSystem(extent={{-140,-220},{220,140}}),
                                                             graphics={
                Rectangle(
                extent={{-140,-120},{240,-140}},
                lineColor={28,108,200},
                lineThickness=1,
                fillColor={170,213,255},
                fillPattern=FillPattern.Solid), Text(
                extent={{-140,-130},{240,-140}},
                lineColor={0,0,255},
                lineThickness=1,
                fillColor={255,170,213},
                fillPattern=FillPattern.None,
                textString="Network Voltages and Powers

")}));
      end OutputsInterfaceBusVoltagesBranchPowers;
    end Interfaces;

    package Linearization
      "Gives an example of basic linearization analysis for the SMIB."
      extends Modelica.Icons.ExamplesPackage;
      package StartHere "\"Please read these notes to use this subpackage\""
        extends Modelica.Icons.Information;
        annotation (Documentation(info="<html>
<p>Before excecuting the functions in this subpackage or running the examples, please make sure that the following flag is disabled.</p>
<ul>
<li>Go to simulation and click on &quot;Simulation Set-up&quot;.</li>
<li>Go to the &quot;Translation&quot; tab.</li>
<li>Disable the flag &quot;Include a variable for elapsed CPU time during simulation&quot;</li>
</ul>
<p><br>If you have issues running the functions &quot;LinearizeSMIB&quot; and &quot;LinearizeSMIBGeneral&quot; it is most likely because the number of output signals increases due to this flag being enabled, which is not considered in the code.</p>
<p>The model &quot;LinearModelExample&quot; can be executed on its own. The model has been parametrized from the values obtained by running the &quot;LinearizeSMIB&quot;, you can see the script and apply it to other cases to understand how the model and data are put together.</p>
<p>Please see the &quot;Documentation&quot; view for the functions:</p>
<ul>
<li>LinearizeSMIB - linearizes the model and runs the non-linear model to obtain the steady state outputs used to parametrize the linear model for simulation.</li>
<li>LinearizeSMIBGeneral - automates the process above and generates results for comparison between the nonlinear and linear model.</li>
</ul>
<p><br>Please see the documentation of each function and the code comments.</p>
</html>"));
      end StartHere;

      function LinearizeSMIB
        // Import things needed for the calculations
        import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
        // Declare outputs to display
        output Real A[:,:] "A-matrix";
        output Real B[:,:] "B-matrix";
        output Real C[:,:] "C-matrix";
        output Real D[:,:] "D-matrix";
        output String inputNames[:] "Modelica names of inputs";
        output String outputNames[:] "Modelica names of outputs";
        output String stateNames[:] "Modelica names of states";
        // Declare reconfigurable inputs
        input Modelica.SIunits.Time tlin = 0 "t for model linearization";
        input Modelica.SIunits.Time tsim = 15 "Simulation time";
        input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.SMIB_GEN_wInput";
        input String pathToNonlinearExperiment=
            "SMIBPS_IdControl.Analysis.LinearAnalysis.PerturbationAnalysis.PerturbGen";
        input String pathToLinearExperiment = "SMIBPS_IdControl.Analysis.LinearAnalysis.Linearization.LinearModelExample";

      algorithm
        // Compute and display the ABCD matrices, etc
        Modelica.Utilities.Streams.print("Linearized Model");
        (A,B,C,D,inputNames,outputNames,stateNames) :=
          Modelica_LinearSystems2.Utilities.Import.linearize(
          pathToNonlinearPlantModel,tlin);
        nx := size(A, 1); //number of states
        Modelica.Utilities.Streams.print("Number of states" + String(nx));
        // Now we want to extract the initial value of the outputs to use it in our
        // linear model response
        Modelica.Utilities.Streams.print("Simulating nonlinear model");
        simulateModel(
          pathToNonlinearExperiment,
          stopTime=tsim,
          numberOfIntervals=1000,
          resultFile="res_nl");
         ny := size(C, 1);
         y0 := DymolaCommands.Trajectories.readTrajectory(
           "res_nl.mat",
           {outputNames[i] for i in 1:ny},
           DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat"));
         DataFiles.writeMATmatrix(
           "MyData.mat",
           "y0",
           [y0[1:ny,1]],
           append=true);
        // Print y0's first values which is needed for the linear response model
        y0out := y0[:,1]; // we only want the first few elements
        Modelica.Utilities.Streams.print("y0 =");
        Modelica.Math.Vectors.toString(y0out);
        annotation(__Dymola_interactive=true);
      end LinearizeSMIB;

      model LinearModelExample
        "Simulate the linearized SMIB model obtained by running the function LinearizeSMIB."
        extends Modelica.Icons.Example;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        Modelica.Blocks.Sources.Step stepEfd(height=0.01, startTime=1)
          annotation (Placement(transformation(extent={{-124,22},{-104,42}})));
        inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
          annotation (Placement(transformation(extent={{72,70},{92,90}})));
        Modelica.Blocks.Routing.Multiplex3 multiplex3_1(n1=1, n2=1)
          annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
        Modelica_LinearSystems2.Controller.StateSpace stateSpace(system(
            A=[0.0,376.99111838914104,0.0,0.0,0.0,0.0; -0.15935012502522555,0.0,
                0.0,0.0,-0.1730203086974122,0.06000759113665713; -0.225786591276862,
                0.0,-0.12499999999582975,0.0,-0.2668898899908987,-0.0011041056183099536;
                0.46107962381245693,0.0,0.0,-1.0000000000758573,
                0.006336424569356028,-1.4894289460374501; -2.9728307608838804,
                0.0,33.33333333222127,0.0,-36.84735279230479,-0.014537265192078836;
                2.6215590291419555,0.0,0.0,14.285714286797962,
                0.0360269902464371,-22.754155959037135],
            B=[0.0,0.0,0.0; 0.0,0.14285715467720342,-0.031538676432360496;
                0.12500001034254637,0.0,-0.04074007797782997; 0.0,0.0,
                0.09771200515373835; 0.0,0.0,-0.5364063360158156; 0.0,0.0,
                0.5555611526375515],
            C=[-0.13790065289076472,0.0,0.0,0.0,0.4364388135635919,
                0.4994401309217214; 0.3122089845885527,0.0,0.0,0.0,
                1.4657741561523794,0.7706076485898833; 1.2147747709609267,0.0,
                0.0,0.0,1.371465981949742,-0.3580020102583231; 0.0,
                0.999999999889562,0.0,0.0,0.0,0.0; 0.9999999998632589,0.0,0.0,
                0.0,0.0,0.0],
            D=[0.0,0.0,-0.015863310665054087; 0.0,0.0,0.02177458213736827; 0.0,
                0.0,0.21947843542591272; 0.0,0.0,0.0; 0.0,0.0,0.0],
            yNames={"Vt","Q","P","w","delta"},
            xNames={"G1.machine.delta","G1.machine.w","G1.machine.e1q",
                "G1.machine.e1d","G1.machine.e2q","G1.machine.e2d"},
            uNames={"uEfd","uPm","uPload"}),
                                    blockType=Modelica_LinearSystems2.Controller.Types.BlockTypeWithGlobalDefault.Continuous)
          annotation (Placement(transformation(extent={{-36,-10},{-16,10}})));
        Modelica.Blocks.Routing.DeMultiplex5 demultiplex2_2
          annotation (Placement(transformation(extent={{60,-20},{100,20}})));
        Modelica.Blocks.Math.Add addy[5]
          annotation (Placement(transformation(extent={{6,-16},{26,4}})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0)
          annotation (Placement(transformation(extent={{-124,-10},{-104,10}})));
        Modelica.Blocks.Routing.Multiplex5 multiplex5_1 annotation (Placement(
              transformation(
              extent={{-14,-14},{14,14}},
              rotation=90,
              origin={2,-38})));
        Modelica.Blocks.Sources.Constant y0_Vt(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-80,-90})));
        Modelica.Blocks.Sources.Constant y0_Q(k=0.4360022246837616)
                                                          annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-40,-90})));
        Modelica.Blocks.Sources.Constant y0_P(k=0.8999999761581421)
                                                     annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={0,-90})));
        Modelica.Blocks.Sources.Constant y0_w(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={40,-90})));
        Modelica.Blocks.Sources.Constant y0_delta(k=1.2242474555969238)
                                                             annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={80,-90})));
        Modelica.Blocks.Sources.Constant Ploadchange(k=0) annotation (Placement(
              transformation(extent={{-126,-42},{-106,-22}})));
      equation
        connect(multiplex3_1.u1[1], stepEfd.y) annotation (Line(points={{-82,7},
                {-90,7},{-90,32},{-103,32}},
                                    color={0,0,127}));
        connect(multiplex3_1.y, stateSpace.u)
          annotation (Line(points={{-59,0},{-38,0}}, color={0,0,127}));
        connect(demultiplex2_2.y1[1], Vt) annotation (Line(points={{102,16},{
                108,16},{108,80},{150,80}},
                               color={0,0,127}));
        connect(demultiplex2_2.y4[1], w) annotation (Line(points={{102,-8},{110,
                -8},{110,-40},{150,-40}},
                                 color={0,0,127}));
        connect(demultiplex2_2.y5[1], delta) annotation (Line(points={{102,-16},
                {108,-16},{108,-80},{150,-80}},
                                      color={0,0,127}));
        connect(addy.y, demultiplex2_2.u)
          annotation (Line(points={{27,-6},{28,-6},{28,0},{56,0}}, color={0,0,127}));
        connect(stateSpace.y, addy.u1)
          annotation (Line(points={{-15,0},{4,0}}, color={0,0,127}));
        connect(Pmchange.y,multiplex3_1. u2[1]) annotation (Line(points={{-103,0},
                {-82,0}},                color={0,0,127}));
        connect(multiplex5_1.y, addy.u2)
          annotation (Line(points={{2,-22.6},{2,-12},{4,-12}}, color={0,0,127}));
        connect(multiplex5_1.u1[1], y0_Vt.y) annotation (Line(points={{-12,-54.8},{-80,
                -54.8},{-80,-79}}, color={0,0,127}));
        connect(y0_Q.y, multiplex5_1.u2[1]) annotation (Line(points={{-40,-79},{-40,-64},
                {-5,-64},{-5,-54.8}}, color={0,0,127}));
        connect(y0_P.y, multiplex5_1.u3[1])
          annotation (Line(points={{0,-79},{2,-79},{2,-54.8}}, color={0,0,127}));
        connect(y0_w.y, multiplex5_1.u4[1]) annotation (Line(points={{40,-79},{40,-64},
                {9,-64},{9,-54.8}}, color={0,0,127}));
        connect(y0_delta.y, multiplex5_1.u5[1]) annotation (Line(points={{80,-79},{80,
                -54.8},{16,-54.8}}, color={0,0,127}));
        connect(demultiplex2_2.y3[1], P) annotation (Line(points={{102,0},{122,
                0},{122,40},{150,40}},     color={0,0,127}));
        connect(Q, demultiplex2_2.y2[1]) annotation (Line(points={{150,0},{126,
                0},{126,8},{102,8}},     color={0,0,127}));
        connect(Ploadchange.y, multiplex3_1.u3[1]) annotation (Line(points={{
                -105,-32},{-96,-32},{-96,-7},{-82,-7}}, color={0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false), graphics={
              Text(
                extent={{-58,24},{-44,4}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                textString="du",
                fontSize=24),
              Text(
                extent={{-38,-18},{-22,-38}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                textString="y0",
                fontSize=24),
              Text(
                extent={{40,20},{50,4}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                fontSize=24,
                textString="y"),
              Text(
                extent={{-12,22},{2,6}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                fontSize=24,
                textString="dy"),
              Text(
                extent={{-80,80},{0,60}},
                lineColor={85,170,255},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={28,108,200},
                horizontalAlignment=TextAlignment.Left,
                textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
              Line(
                points={{-4,64},{24,44},{18,14}},
                color={0,0,255},
                thickness=0.5,
                arrow={Arrow.None,Arrow.Filled},
                smooth=Smooth.Bezier)}),
          experiment(
            StopTime=15,
            __Dymola_NumberOfIntervals=10000,
            __Dymola_fixedstepsize=0.01,
            __Dymola_Algorithm="Rkfix4"));
      end LinearModelExample;

      function LinearizeSMIBGeneral
        "Makes the approach shown in the LinearModelExample and LinearizeSMIB more general."
        // See the Documentation for an explanation of the goals.
        // IMPORTING FUNCTIONS
        // Import things needed for the calculations
        import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
        // OUTPUTS OF THEFUNCTION - FOR DISPLAY
        // Declare outputs to display
        output Real A[:,:] "A-matrix";
        output Real B[:,:] "B-matrix";
        output Real C[:,:] "C-matrix";
        output Real D[:,:] "D-matrix";
        output String inputNames[:] "Modelica names of inputs";
        output String outputNames[:] "Modelica names of outputs";
        output String stateNames[:] "Modelica names of states";
        output Real y0out[:] "Initial value of the output variables";
        // INPUTS TO THE FUNCTION
        // Declare reconfigurable simulation parameters
        input Modelica.SIunits.Time tlin = 0 "t for model linearization";
        input Modelica.SIunits.Time tsim = 15 "Simulation time";
        input Real numberOfIntervalsin=10000 "No. of intervals";
        // Use this for Case A
        //input String method = "Rkfix4" "Solver";
        input String methodin = "DASSL" "Solver";
        input Real fixedstepsizein= 0.01 "Time step - needed only for fixed time step solvers";
        //
        // DEFINING THE NONLINEAR PLANT, NONLINEAR EXPERIMENT, AND LINEAR EXPERIMENT MODELS
        //
        // 1) NONLINEAR PLANT: this is a model with input and outputs definde and under the .Analysis.LinearAnalysis.Interfaces sub-package
        // This is the model that will be linearized, i.e. the nonlinear plant model
        // The default is the model with constant Efd and Pm
        // commented, are the other two cases for AVR and AVR+PSS
        // Case A: with no controls - i.e. constant Efd and Pm
        // input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.SMIB_GEN_wInput" "Nonlinear plant model";
        // Case B: with AVR only
        // input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.SMIB_AVR_wInput" "Nonlinear model";
        // Case C: with AVR+PSS
         input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.SMIB_AVR_PSS_wInput" "Nonlinear plant model";
        //
        //
        // 2) NONLINEAR EXPERIMENT: this is a model which applies a change to the input of the nonlinear model.
        // It must match the nonlinar plant above. These models are under .Analysis.LinearAnalysis.PerturbationAnalysis
        // This model will be simulated, and the simulation results will be compared to the simulation of the corresponding linearized model.
        // Case A: with no controls - constant Efd and Pm
        // input String pathToNonlinearExperiment= "SMIBPS_IdControl.Analysis.LinearAnalysis.PerturbationAnalysis.PerturbGen" "Nonlinear experiment model";
        // Case B: with AVR only
        // input String pathToNonlinearExperiment= "SMIBPS_IdControl.Analysis.LinearAnalysis.PerturbationAnalysis.PerturbAVR" "Nonlinear experiment model";
        // Case C: with AVR+PSS
         input String pathToNonlinearExperiment= "SMIBPS_IdControl.Analysis.LinearAnalysis.PerturbationAnalysis.PerturbPSS" "Nonlinear experiment model";
        //
        //
        // 3) LINEAR EXPERIMENT: this is a template that can be used for all three cases, so it is not necessary to create other cases here
        input String pathToLinearExperiment = "SMIBPS_IdControl.Analysis.LinearAnalysis.Linearization.LinearModelGeneral";

      algorithm
        // Compute and display the ABCD matrices, etc
        (A,B,C,D,inputNames,outputNames,stateNames) :=
          Modelica_LinearSystems2.Utilities.Import.linearize(
          pathToNonlinearPlantModel,tlin);
        // LINEARIZE plant model at t_lin
        // This is the same as above, however, it stores it in a StateSpace object
        ss := Modelica_LinearSystems2.ModelAnalysis.Linearize(
          pathToNonlinearPlantModel, simulationSetup=
          Modelica_LinearSystems2.Records.SimulationOptionsForLinearization(
          linearizeAtInitial=false, t_linearize=tlin));
        // PRINT linear system
        Modelica.Utilities.Streams.print(String(ss));
        // SAVE the data in a mat file
        DataFiles.writeMATmatrix(
          "MyData.mat",
          "ABCD",
          [ss.A, ss.B; ss.C, ss.D],
          append=false);
        nx := size(ss.A, 1);
        DataFiles.writeMATmatrix(
          "MyData.mat",
          "nx",
          [nx],
          append=true);
        Modelica.Utilities.Streams.print("Simulating nonlinear model");
        simulateModel(
          pathToNonlinearExperiment,
          stopTime=tsim,
          numberOfIntervals=numberOfIntervalsin, method = methodin, fixedstepsize=fixedstepsizein,
          resultFile="res_nl");
         ny := size(ss.C, 1);
         y0 := DymolaCommands.Trajectories.readTrajectory(
           "res_nl.mat",
           {ss.yNames[i] for i in 1:ny},
           DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat"));
          // {"PS_ConstantEfd." + ss.yNames[i] for i in 1:ny},
         DataFiles.writeMATmatrix(
           "MyData.mat",
           "y0",
           [y0[1:ny,1]],
           append=true);
        // Print y0's first values which is needed for the linear response model
        y0out := y0[:,1]; // we only want the first few elements
        Modelica.Utilities.Streams.print("y0 =");
        Modelica.Math.Vectors.toString(y0out);
        //
        // We now simulate the linear model, which requires y0
        Modelica.Utilities.Streams.print("Simulating linear model");
        simulateModel(
          pathToLinearExperiment,
          stopTime=tsim,
          numberOfIntervals=numberOfIntervalsin, method = methodin, fixedstepsize=fixedstepsizein,
          resultFile="res_lin");
        annotation(__Dymola_interactive=true, Documentation(info="<html>
<p>This&nbsp;function&nbsp;will&nbsp;take&nbsp;in&nbsp;the&nbsp;nonlinear&nbsp;plant&nbsp;model,&nbsp;nonlinear&nbsp;experiments,&nbsp;and&nbsp;a&nbsp;linear&nbsp;model&nbsp;template.</p>
<p><br>It&nbsp;will&nbsp;linearize&nbsp;the&nbsp;nonlinear&nbsp;plant,&nbsp;use&nbsp;the&nbsp;linear&nbsp;model&nbsp;for&nbsp;simulation&nbsp;in&nbsp;a&nbsp;linear&nbsp;model&nbsp;experiment,&nbsp;and&nbsp;run&nbsp;the&nbsp;nonlinear&nbsp;model.</p>
<p><br>The&nbsp;function&nbsp;has&nbsp;been&nbsp;designed&nbsp;so&nbsp;that&nbsp;only&nbsp;the&nbsp;nonlinear&nbsp;plant&nbsp;and&nbsp;nonlinear&nbsp;experiment&nbsp;have&nbsp;to&nbsp;be&nbsp;specified.</p>
<p>The goal is to show the limits of linearized models, including when typical controllers are represented.</p>
<p>Three cases are included:</p>
<p style=\"margin-left: 30px;\">- Case A: Constant Efd and Pm, i.e. no controls</p>
<p style=\"margin-left: 30px;\">- Case B: AVR only</p>
<p style=\"margin-left: 30px;\">- Case C: AVR+PSS</p>
<p>To analyze each of these cases, the function source code can be commented out, for example, the default is Case A, which is:</p>
<p><span style=\"font-family: Courier New;\">&nbsp;&nbsp;<span style=\"color: #006400;\">//&nbsp;Case&nbsp;A:&nbsp;with&nbsp;no&nbsp;controls&nbsp;-&nbsp;i.e.&nbsp;constant&nbsp;Efd&nbsp;and&nbsp;Pm&nbsp;</span></p>
<p><span style=\"font-family: Courier New;\">&nbsp;&nbsp;<span style=\"color: #0000ff;\">input&nbsp;</span><span style=\"color: #ff0000;\">String</span>&nbsp;pathToNonlinearPlantModel&nbsp;=&nbsp;&quot;SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.SMIB_GEN_wInput&quot;&nbsp;<span style=\"font-family: Courier New; color: #006400;\">&quot;Nonlinear&nbsp;plant&nbsp;model&quot;</span>;</p>
<p><span style=\"font-family: Courier New;\">&nbsp;&nbsp;<span style=\"color: #006400;\">//&nbsp;Case&nbsp;A:&nbsp;with&nbsp;no&nbsp;controls&nbsp;-&nbsp;constant&nbsp;Efd&nbsp;and&nbsp;Pm</span></p>
<p><span style=\"font-family: Courier New;\">&nbsp;&nbsp;<span style=\"color: #0000ff;\">input&nbsp;</span><span style=\"color: #ff0000;\">String</span>&nbsp;pathToNonlinearExperiment=&nbsp;&quot;SMIBPS_IdControl.Analysis.LinearAnalysis.PerturbationAnalysis.PerturbGen&quot;&nbsp;<span style=\"font-family: Courier New; color: #006400;\">&quot;Nonlinear&nbsp;experiment&nbsp;model&quot;</span>;</p>
<p><br>Commenting the source code line above and uncommenting the following line in the source code would allow to analyze Case B:</p>
<p><span style=\"font-family: Courier New;\">&nbsp;&nbsp;<span style=\"color: #006400;\">//&nbsp;Case&nbsp;B:&nbsp;with&nbsp;AVR&nbsp;only</span></p>
<p><span style=\"font-family: Courier New;\">&nbsp;&nbsp;<span style=\"color: #0000ff;\">input&nbsp;</span><span style=\"color: #ff0000;\">String</span>&nbsp;pathToNonlinearPlantModel&nbsp;=&nbsp;&quot;SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.SMIB_AVR_wInput&quot;&nbsp;<span style=\"font-family: Courier New; color: #006400;\">&quot;Nonlinear&nbsp;model&quot;</span>;</p>
<p><span style=\"font-family: Courier New;\">&nbsp;&nbsp;<span style=\"color: #006400;\">//&nbsp;Case&nbsp;B:&nbsp;with&nbsp;AVR&nbsp;only</span></p>
<p><span style=\"font-family: Courier New;\">&nbsp;&nbsp;<span style=\"color: #0000ff;\">input&nbsp;</span><span style=\"color: #ff0000;\">String</span>&nbsp;pathToNonlinearExperiment=&nbsp;&quot;SMIBPS_IdControl.Analysis.LinearAnalysis.PerturbationAnalysis.PerturbAVR&quot;&nbsp;<span style=\"font-family: Courier New; color: #006400;\">&quot;Nonlinear&nbsp;experiment&nbsp;model&quot;</span>;</p>
<p><br><br>This can be similarly done for case C.</p>
<p><br><b>Note:</b></p>
<p>If the amplitude and time of the step change needs to be modified, they must be changed in the &quot;LinearModelGeneral&quot; model and in each of the &quot;PerturbXYZ&quot; models under perturbation analysis.</p>
<p><br><b>To do:</b></p>
<p>- allow to modify as a parameter the height of the step for the nonlinear and linear experiments.</p>
<p>- include perturbations on Pm?</p>
<p>- other things?</p>
</html>"));
      end LinearizeSMIBGeneral;

      model LinearModelGeneral
        "Simulate the linearized SMIB model obtained by running the function LinearizeSMIB."
        extends SMIBPS_IdControl.Utilities.Icons.FunctionDependentExample;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        // The following definitions are very important to couple the linear model
        // to the linearization of the nonlinear model and the simulation
        parameter Real[:] y0=vector(DataFiles.readMATmatrix("MyData.mat", "y0")) annotation (Evaluate=false);
        // The following has to be imported in order to be able to interpret and manipulate the StateSpace types
        import Modelica_LinearSystems2.StateSpace;
        parameter StateSpace ss=StateSpace.Import.fromFile("MyData.mat", "ABCD");
        parameter Integer ny=size(ss.C, 1);
        Modelica.Blocks.Sources.Step step_voltage_input(height=0.01, startTime=
              1)
          annotation (Placement(transformation(extent={{-120,22},{-100,42}})));
        inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
          annotation (Placement(transformation(extent={{60,60},{80,80}})));
        Modelica.Blocks.Routing.Multiplex3 multiplex3_1(n1=1, n2=1)
          annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
        Modelica.Blocks.Routing.DeMultiplex5 demultiplex2_2
          annotation (Placement(transformation(extent={{60,-20},{100,20}})));
        Modelica.Blocks.Math.Add addy[ny]
          annotation (Placement(transformation(extent={{6,-16},{26,4}})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0)
          annotation (Placement(transformation(extent={{-122,-10},{-102,10}})));
        Modelica.Blocks.Sources.Constant y0_initial[ny](k=y0)      annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={0,-32})));
        Modelica_LinearSystems2.Controller.StateSpace stateSpace(system=ss)
          annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
        Modelica.Blocks.Sources.Constant Ploadchange(k=0) annotation (Placement(
              transformation(extent={{-122,-50},{-102,-30}})));
      equation
        connect(multiplex3_1.u1[1], step_voltage_input.y) annotation (Line(
              points={{-82,7},{-90,7},{-90,32},{-99,32}}, color={0,0,127}));
        connect(demultiplex2_2.y1[1], Vt) annotation (Line(points={{102,16},{108,16},{
                108,79},{150,79}},
                               color={0,0,127}));
        connect(demultiplex2_2.y4[1], w) annotation (Line(points={{102,-8},{110,-8},{110,
                -39},{150,-39}}, color={0,0,127}));
        connect(demultiplex2_2.y5[1], delta) annotation (Line(points={{102,-16},{108,-16},
                {108,-81},{150,-81}}, color={0,0,127}));
        connect(addy.y, demultiplex2_2.u)
          annotation (Line(points={{27,-6},{28,-6},{28,0},{56,0}}, color={0,0,127}));
        connect(Pmchange.y,multiplex3_1. u2[1]) annotation (Line(points={{-101,0},
                {-82,0}},                color={0,0,127}));
        connect(demultiplex2_2.y3[1], P) annotation (Line(points={{102,0},{122,0},{122,
                41},{150,41}}, color={0,0,127}));
        connect(Q, demultiplex2_2.y2[1]) annotation (Line(points={{150,1},{126,1},{126,
                8},{102,8}}, color={0,0,127}));
        connect(y0_initial.y, addy.u2)
          annotation (Line(points={{0,-21},{0,-12},{4,-12}}, color={0,0,127}));
        connect(multiplex3_1.y, stateSpace.u)
          annotation (Line(points={{-59,0},{-42,0}}, color={0,0,127}));
        connect(stateSpace.y, addy.u1)
          annotation (Line(points={{-19,0},{4,0}}, color={0,0,127}));
        connect(Ploadchange.y, multiplex3_1.u3[1]) annotation (Line(points={{
                -101,-40},{-94,-40},{-94,-7},{-82,-7}}, color={0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false), graphics={
              Text(
                extent={{-58,24},{-44,4}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                textString="du",
                fontSize=24),
              Text(
                extent={{-12,20},{-2,4}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                textString="dy",
                fontSize=24),
              Text(
                extent={{-38,-18},{-22,-38}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                textString="y0",
                fontSize=24),
              Text(
                extent={{40,20},{50,4}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                fontSize=24,
                textString="y"),
              Text(
                extent={{-80,80},{0,60}},
                lineColor={85,170,255},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={28,108,200},
                horizontalAlignment=TextAlignment.Left,
                textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
              Text(
                extent={{-80,-80},{80,-100}},
                lineColor={85,170,255},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={28,108,200},
                horizontalAlignment=TextAlignment.Left,
                textString="Notice the change in the order of the outputs w.r.t. the nonlinear model.
They have to be rearranged based on the order provided by the linearization function."),
              Line(
                points={{66,-86},{106,-84},{120,-4}},
                color={0,0,255},
                thickness=0.5,
                arrow={Arrow.None,Arrow.Filled},
                smooth=Smooth.Bezier),
              Line(
                points={{-4,68},{24,48},{18,18}},
                color={0,0,255},
                thickness=0.5,
                arrow={Arrow.None,Arrow.Filled},
                smooth=Smooth.Bezier)}),
          experiment(
            StopTime=15,
            __Dymola_NumberOfIntervals=1000,
            __Dymola_Algorithm="Dassl"),
          Documentation(info="<html>
<p>DO NOT try to run this model on it&apos;s own! </p>
<p>Models with this icon will not simulate on their own, instead they work together with a function that populates certain parameters in the model and perform other operations.</p>
<p><br>See the associated function to run:<span style=\"color: #1c6cc8;\"> <span style=\"font-family: Courier New; font-size: 8pt;\">LinearizeSMIBGeneral </span>within <span style=\"font-family: Courier New; font-size: 8pt; color: #1c6cc8;\">SMIB_PSControl.Analysis.LinearAnalysis.BasicLinearization.LinearizeSMIBGeneral()</span></p>
</html>"));
      end LinearModelGeneral;
    end Linearization;

    package PerturbationAnalysis
      "Examples that perform perturbations on the input of different model variants for comparison between linear and nonlinear models."
      extends Modelica.Icons.VariantsPackage;
      model PerturbGen
        "Simulates the nonlinear SMIB model with a 1% step in the field voltage input."
        extends Modelica.Icons.Example;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        Modelica.Blocks.Sources.Step stepEfd(height=0.01, startTime=1)
          annotation (Placement(transformation(extent={{-120,10},{-100,30}})));
        Interfaces.SMIB_GEN_wInput PS_ConstantEfd
          annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
        Modelica.Blocks.Math.Gain efdInputGain(k=1)  annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,20})));
        Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-72,-20})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0) annotation (Placement(
              transformation(extent={{-120,-30},{-100,-10}})));
        Modelica.Blocks.Sources.Constant Ploadchange(k=0)
          annotation (Placement(transformation(extent={{-118,-70},{-98,-50}})));
        Modelica.Blocks.Math.Gain uPloadInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,-60})));
      equation
        connect(Vt, PS_ConstantEfd.Vt) annotation (Line(points={{150,79},{98,79},
                {98,22.5714},{42.8571,22.5714}},
                                    color={0,0,127}));
        connect(P, PS_ConstantEfd.P) annotation (Line(points={{150,41},{112,41},
                {112,11.7143},{42.8571,11.7143}},
                                    color={0,0,127}));
        connect(Q, PS_ConstantEfd.Q) annotation (Line(points={{150,1},{96,1},{
                96,0.285714},{42.8571,0.285714}},
                                     color={0,0,127}));
        connect(w, PS_ConstantEfd.w) annotation (Line(points={{150,-39},{108,
                -39},{108,-11.1429},{42.8571,-11.1429}},
                                               color={0,0,127}));
        connect(delta, PS_ConstantEfd.delta) annotation (Line(points={{150,-81},
                {92,-81},{92,-23.1429},{42.8571,-23.1429}},
                                                   color={0,0,127}));
        connect(efdInputGain.u, stepEfd.y)
          annotation (Line(points={{-82,20},{-99,20}}, color={0,0,127}));
        connect(pmInputGain.y, PS_ConstantEfd.uPm) annotation (Line(points={{-61,-20},
                {-54,-20},{-54,-11.4286},{-45.7143,-11.4286}}, color={0,0,127}));
        connect(efdInputGain.y, PS_ConstantEfd.uEfd) annotation (Line(points={{-59,20},
                {-52,20},{-52,11.4286},{-45.7143,11.4286}}, color={0,0,127}));
        connect(Pmchange.y, pmInputGain.u)
          annotation (Line(points={{-99,-20},{-84,-20}}, color={0,0,127}));
        connect(Ploadchange.y, uPloadInputGain.u)
          annotation (Line(points={{-97,-60},{-82,-60}}, color={0,0,127}));
        connect(uPloadInputGain.y, PS_ConstantEfd.uPload) annotation (Line(
              points={{-59,-60},{-56,-60},{-56,-28.5714},{-45.7143,-28.5714}},
              color={0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false)),
          experiment(
            StopTime=15,
            __Dymola_fixedstepsize=0.01,
            __Dymola_Algorithm="Rkfix2"));
      end PerturbGen;

      model PerturbAVR
        "Simulates the nonlinear SMIB model with a 1% step in the Vs input."
        extends Modelica.Icons.Example;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        Modelica.Blocks.Sources.Step stepVs(height=0.01,  startTime=1)
          annotation (Placement(transformation(extent={{-120,10},{-100,30}})));
        Interfaces.SMIB_AVR_wInput PS_wAVR
          annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
        Modelica.Blocks.Math.Gain vsInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,20})));
        Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,-20})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0) annotation (Placement(
              transformation(extent={{-120,-30},{-100,-10}})));
        Modelica.Blocks.Sources.Constant Ploadchange(k=0) annotation (Placement(
              transformation(extent={{-122,-70},{-102,-50}})));
        Modelica.Blocks.Math.Gain uPloadInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-74,-60})));
      equation
        connect(Vt, PS_wAVR.Vt) annotation (Line(points={{150,80},{98,80},{98,
                22.8571},{42.8571,22.8571}},
                                    color={0,0,127}));
        connect(P, PS_wAVR.P) annotation (Line(points={{150,40},{112,40},{112,
                11.4286},{42.8571,11.4286}},
                                    color={0,0,127}));
        connect(Q, PS_wAVR.Q) annotation (Line(points={{150,0},{96,0},{96,0},{
                42.8571,0}},color={0,0,127}));
        connect(w, PS_wAVR.w) annotation (Line(points={{150,-40},{108,-40},{108,
                -11.4286},{42.8571,-11.4286}},
                                     color={0,0,127}));
        connect(delta, PS_wAVR.delta) annotation (Line(points={{150,-80},{92,
                -80},{92,-22.8571},{42.8571,-22.8571}},
                                               color={0,0,127}));
        connect(vsInputGain.u, stepVs.y)
          annotation (Line(points={{-82,20},{-99,20}}, color={0,0,127}));
        connect(vsInputGain.y, PS_wAVR.uVref) annotation (Line(points={{-59,20},
                {-54,20},{-54,22.8571},{-45.7143,22.8571}},
                                                   color={0,0,127}));
        connect(pmInputGain.y, PS_wAVR.uPm) annotation (Line(points={{-59,-20},
                {-54,-20},{-54,0},{-45.7143,0}},     color={0,0,127}));
        connect(Pmchange.y, pmInputGain.u)
          annotation (Line(points={{-99,-20},{-82,-20}}, color={0,0,127}));
        connect(Ploadchange.y, uPloadInputGain.u)
          annotation (Line(points={{-101,-60},{-86,-60}}, color={0,0,127}));
        connect(uPloadInputGain.y, PS_wAVR.uPload) annotation (Line(points={{-63,-60},
                {-56,-60},{-56,-22.8571},{-45.7143,-22.8571}},          color={
                0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false)),
          experiment(
            StopTime=15,
            __Dymola_fixedstepsize=0.01,
            __Dymola_Algorithm="Rkfix4"));
      end PerturbAVR;

      model PerturbPSS
        "Simulates the nonlinear SMIB model with a 1% step in the PSS input."
        extends Modelica.Icons.Example;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        Modelica.Blocks.Sources.Step stepPSS(height=0.01, startTime=1)
          annotation (Placement(transformation(extent={{-120,10},{-100,30}})));
        Interfaces.SMIB_AVR_PSS_wInput PS_wPSS
          annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
        Modelica.Blocks.Math.Gain pSSInputGain(k=1)  annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,20})));
        Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,-20})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0) annotation (Placement(
              transformation(extent={{-120,-30},{-100,-10}})));
        Modelica.Blocks.Sources.Constant Ploadchange(k=0) annotation (Placement(
              transformation(extent={{-120,-70},{-100,-50}})));
        Modelica.Blocks.Math.Gain uPloadInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-72,-60})));
      equation
        connect(Vt, PS_wPSS.Vt) annotation (Line(points={{150,79},{98,79},{98,
                22.5714},{42.8571,22.5714}},
                                    color={0,0,127}));
        connect(P, PS_wPSS.P) annotation (Line(points={{150,41},{112,41},{112,
                11.7143},{42.8571,11.7143}},
                                    color={0,0,127}));
        connect(Q, PS_wPSS.Q) annotation (Line(points={{150,1},{96,1},{96,
                0.285714},{42.8571,0.285714}},
                                     color={0,0,127}));
        connect(w, PS_wPSS.w) annotation (Line(points={{150,-39},{108,-39},{108,
                -11.1429},{42.8571,-11.1429}},
                                     color={0,0,127}));
        connect(delta, PS_wPSS.delta) annotation (Line(points={{150,-81},{92,
                -81},{92,-23.1429},{42.8571,-23.1429}},
                                               color={0,0,127}));
        connect(pSSInputGain.u, stepPSS.y)
          annotation (Line(points={{-82,20},{-99,20}}, color={0,0,127}));
        connect(pmInputGain.y, PS_wPSS.uPm) annotation (Line(points={{-59,-20},
                {-54,-20},{-54,-11.4286},{-45.7143,-11.4286}},
                                                          color={0,0,127}));
        connect(pSSInputGain.y, PS_wPSS.uPSS) annotation (Line(points={{-59,20},
                {-54,20},{-54,11.4286},{-45.7143,11.4286}},
                                                       color={0,0,127}));
        connect(Pmchange.y, pmInputGain.u)
          annotation (Line(points={{-99,-20},{-82,-20}}, color={0,0,127}));
        connect(Ploadchange.y, uPloadInputGain.u)
          annotation (Line(points={{-99,-60},{-84,-60}}, color={0,0,127}));
        connect(uPloadInputGain.y, PS_wPSS.uPload) annotation (Line(points={{-61,-60},
                {-60,-60},{-60,-28.5714},{-45.7143,-28.5714}},          color={
                0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false)),
          experiment(
            StopTime=15,
            __Dymola_fixedstepsize=0.01,
            __Dymola_Algorithm="Radau"),
          Documentation(info="<html>
<p><span style=\"font-size: 10pt;\">This model applies a 1&percnt; step change in the PSS input signal.</span></p>
<p><span style=\"font-size: 10pt;\">It serves to compare the nonlinear response (this simulation) with other nonlinear models.</span></p>
<p><span style=\"font-size: 10pt;\">It also serves to compare it to the linearized model response.</span></p>
<p><span style=\"font-size: 10pt;\">It helps in understanding the automated analysis function which linearizes and compares all model variants.</span></p>
</html>"));
      end PerturbPSS;

      model PerturbPm
        "Simulates the nonlinear SMIB model with a 1% step in the mechanical power input."
        extends Modelica.Icons.Example;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        Modelica.Blocks.Sources.Constant constEfd(k=0)
          annotation (Placement(transformation(extent={{-120,10},{-100,30}})));
        Interfaces.SMIB_GEN_wInput PS_ConstantPm
          annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
        Modelica.Blocks.Math.Gain efdInputGain(k=1)  annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,20})));
        Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,-20})));
        Modelica.Blocks.Sources.Step     Pmchange(height=0.01, startTime=1)
                                                       annotation (Placement(
              transformation(extent={{-118,-30},{-98,-10}})));
        Modelica.Blocks.Sources.Constant Ploadchange(k=0) annotation (Placement(
              transformation(extent={{-120,-70},{-100,-50}})));
        Modelica.Blocks.Math.Gain uPloadInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-72,-60})));
      equation
        connect(Vt, PS_ConstantPm.Vt) annotation (Line(points={{150,79},{98,79},
                {98,22.5714},{42.8571,22.5714}}, color={0,0,127}));
        connect(P, PS_ConstantPm.P) annotation (Line(points={{150,41},{112,41},
                {112,11.7143},{42.8571,11.7143}}, color={0,0,127}));
        connect(Q, PS_ConstantPm.Q) annotation (Line(points={{150,1},{96,1},{96,
                0.285714},{42.8571,0.285714}}, color={0,0,127}));
        connect(w, PS_ConstantPm.w) annotation (Line(points={{150,-39},{108,-39},
                {108,-11.1429},{42.8571,-11.1429}}, color={0,0,127}));
        connect(delta, PS_ConstantPm.delta) annotation (Line(points={{150,-81},
                {92,-81},{92,-23.1429},{42.8571,-23.1429}}, color={0,0,127}));
        connect(efdInputGain.u, constEfd.y)
          annotation (Line(points={{-82,20},{-99,20}}, color={0,0,127}));
        connect(pmInputGain.y, PS_ConstantPm.uPm) annotation (Line(points={{-59,-20},
                {-54,-20},{-54,-11.4286},{-45.7143,-11.4286}},      color={0,0,
                127}));
        connect(efdInputGain.y, PS_ConstantPm.uEfd) annotation (Line(points={{-59,20},
                {-52,20},{-52,11.4286},{-45.7143,11.4286}},         color={0,0,
                127}));
        connect(Pmchange.y, pmInputGain.u)
          annotation (Line(points={{-97,-20},{-82,-20}}, color={0,0,127}));
        connect(Ploadchange.y, uPloadInputGain.u)
          annotation (Line(points={{-99,-60},{-84,-60}}, color={0,0,127}));
        connect(uPloadInputGain.y, PS_ConstantPm.uPload) annotation (Line(
              points={{-61,-60},{-56,-60},{-56,-28.5714},{-45.7143,-28.5714}},
              color={0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false)),
          experiment(
            StopTime=60,
            __Dymola_NumberOfIntervals=5000,
            Tolerance=1e-05,
            __Dymola_fixedstepsize=0.01,
            __Dymola_Algorithm="Euler"));
      end PerturbPm;
    end PerturbationAnalysis;

    package LinearizeAfterDisturbance
      extends Modelica.Icons.VariantsPackage;
      model NonlinModel_for_Simulation
        "Model that includes a line removal at 5 seconds for linearization at initialization and after the line removal"
        extends Modelica.Icons.Example;
        Modelica.Blocks.Interfaces.RealOutput Vt
          annotation (Placement(transformation(extent={{100,70},{120,90}}),
              iconTransformation(extent={{100,70},{120,90}})));
      public
        Modelica.Blocks.Interfaces.RealOutput Q
          annotation (Placement(transformation(extent={{100,-10},{120,10}}),
              iconTransformation(extent={{100,-10},{120,10}})));
        Modelica.Blocks.Interfaces.RealOutput P
          annotation (Placement(transformation(extent={{100,30},{120,50}}),
              iconTransformation(extent={{100,30},{120,50}})));
        Modelica.Blocks.Interfaces.RealOutput w
          annotation (Placement(transformation(extent={{100,-50},{120,-30}}),
              iconTransformation(extent={{100,-50},{120,-30}})));
        Modelica.Blocks.Interfaces.RealOutput delta
          annotation (Placement(transformation(extent={{100,-90},{120,-70}}),
              iconTransformation(extent={{100,-90},{120,-70}})));
        Modelica.Blocks.Sources.Constant constEfd(k=0)
          annotation (Placement(transformation(extent={{-142,30},{-122,50}})));
        Modelica.Blocks.Math.Gain efdInputGain(k=1)  annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-92,40})));
        Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-92,0})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0) annotation (Placement(
              transformation(extent={{-140,-10},{-120,10}})));
        Modelica.Blocks.Sources.Constant Ploadchange(k=0) annotation (Placement(
              transformation(extent={{-142,-50},{-122,-30}})));
        Modelica.Blocks.Math.Gain uPloadInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-94,-40})));
        Interfaces.SMIB_AVR_PSS_wInput_wLineRmoval
          sMIB_AVR_PSS_wInput_wLineRmoval(t1=0.5)
          annotation (Placement(transformation(extent={{-40,-60},{80,60}})));
      equation
        connect(efdInputGain.u,constEfd. y)
          annotation (Line(points={{-104,40},{-121,40}},
                                                       color={0,0,127}));
        connect(Pmchange.y,pmInputGain. u)
          annotation (Line(points={{-119,0},{-104,0}},   color={0,0,127}));
        connect(Ploadchange.y,uPloadInputGain. u)
          annotation (Line(points={{-121,-40},{-106,-40}},
                                                         color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPSS, efdInputGain.y)
          annotation (Line(points={{-48.5714,34.2857},{-65.1428,34.2857},{
                -65.1428,40},{-81,40}}, color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPm, pmInputGain.y)
          annotation (Line(points={{-48.5714,0},{-81,0}}, color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPload, uPloadInputGain.y)
          annotation (Line(points={{-48.5714,-34.2857},{-65.2857,-34.2857},{
                -65.2857,-40},{-83,-40}}, color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wLineRmoval.Vt, Vt) annotation (Line(points={{84.2857,
                34.2857},{84.2857,55.9286},{110,55.9286},{110,80}},
              color={0,0,127}));
        connect(P, sMIB_AVR_PSS_wInput_wLineRmoval.P) annotation (Line(points={{110,40},
                {100,40},{100,17.1429},{84.2857,17.1429}},          color={0,0,
                127}));
        connect(Q, sMIB_AVR_PSS_wInput_wLineRmoval.Q) annotation (Line(points={{110,0},
                {105,0},{105,0},{84.2857,0}},                       color={0,0,
                127}));
        connect(w, sMIB_AVR_PSS_wInput_wLineRmoval.w) annotation (Line(points={{110,-40},
                {100,-40},{100,-17.1429},{84.2857,-17.1429}},           color={
                0,0,127}));
        connect(delta, sMIB_AVR_PSS_wInput_wLineRmoval.delta) annotation (Line(
              points={{110,-80},{92,-80},{92,-34.2857},{84.2857,-34.2857}},
              color={0,0,127}));
        annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                  -100},{100,100}})),                                  Diagram(
              coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                  100,100}})),
          experiment(
            StopTime=30,
            __Dymola_NumberOfIntervals=10000,
            __Dymola_Algorithm="Dassl"),
          __Dymola_Commands(file="MosScripts/busvoltages.mos" "busvoltages",
              file="MosScripts/outputs_lineswitch.mos" "outputs_lineswitch",
            file="MosScripts/generator_and_controls.mos"
              "generator_and_controls"));
      end NonlinModel_for_Simulation;

      package LinAtZero
        extends Modelica.Icons.ExamplesPackage;
        model NonlinModel_for_Linearization
          extends Modelica.Icons.Example;
          Modelica.Blocks.Interfaces.RealOutput Vt
            annotation (Placement(transformation(extent={{100,70},{120,90}}),
                iconTransformation(extent={{100,70},{120,90}})));
        public
          Modelica.Blocks.Interfaces.RealOutput Q
            annotation (Placement(transformation(extent={{100,-10},{120,10}}),
                iconTransformation(extent={{100,-10},{120,10}})));
          Modelica.Blocks.Interfaces.RealOutput P
            annotation (Placement(transformation(extent={{100,30},{120,50}}),
                iconTransformation(extent={{100,30},{120,50}})));
          Modelica.Blocks.Interfaces.RealOutput w
            annotation (Placement(transformation(extent={{100,-50},{120,-30}}),
                iconTransformation(extent={{100,-50},{120,-30}})));
          Modelica.Blocks.Interfaces.RealOutput delta
            annotation (Placement(transformation(extent={{100,-90},{120,-70}}),
                iconTransformation(extent={{100,-90},{120,-70}})));
          Modelica.Blocks.Interfaces.RealInput uPSS
            annotation (Placement(transformation(extent={{-140,40},{-100,80}})));
          Modelica.Blocks.Interfaces.RealInput uPm
            annotation (Placement(transformation(extent={{-140,-20},{-100,20}}),
                iconTransformation(extent={{-140,-20},{-100,20}})));
          Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
                transformation(extent={{-140,-80},{-100,-40}})));
          Interfaces.SMIB_AVR_PSS_wInput_wLineRmoval
            sMIB_AVR_PSS_wInput_wLineRmoval
            annotation (Placement(transformation(extent={{-20,-20},{40,40}})));
        equation
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPSS, uPSS) annotation (Line(
                points={{-24.2857,27.1429},{-69.5714,27.1429},{-69.5714,60},{
                  -120,60}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPm, uPm) annotation (Line(
                points={{-24.2857,10},{-70,10},{-70,0},{-120,0}}, color={0,0,
                  127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPload, uPload) annotation (
              Line(points={{-24.2857,-7.14286},{-67.1428,-7.14286},{-67.1428,
                  -60},{-120,-60}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.Vt, Vt) annotation (Line(
                points={{42.1429,27.1429},{73.0714,27.1429},{73.0714,80},{110,
                  80}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.P, P) annotation (Line(points={{42.1429,
                  18.5714},{72.0714,18.5714},{72.0714,40},{110,40}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.Q, Q) annotation (Line(points={{42.1429,
                  10},{73.0714,10},{73.0714,0},{110,0}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.w, w) annotation (Line(points={{42.1429,
                  1.42857},{73.0714,1.42857},{73.0714,-40},{110,-40}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.delta, delta) annotation (
              Line(points={{42.1429,-7.14286},{42.1429,-44.6786},{110,-44.6786},
                  {110,-80}}, color={0,0,127}));
          annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
                coordinateSystem(preserveAspectRatio=false)));
        end NonlinModel_for_Linearization;

        model NonlinModel_for_NonlinExperiment
          extends Modelica.Icons.Example;
          Modelica.Blocks.Interfaces.RealOutput Vt
            annotation (Placement(transformation(extent={{98,68},{118,90}})));
        public
          Modelica.Blocks.Interfaces.RealOutput Q
            annotation (Placement(transformation(extent={{98,-10},{118,12}})));
          Modelica.Blocks.Interfaces.RealOutput P
            annotation (Placement(transformation(extent={{98,30},{118,52}})));
          Modelica.Blocks.Interfaces.RealOutput w
            annotation (Placement(transformation(extent={{98,-50},{118,-28}})));
          Modelica.Blocks.Interfaces.RealOutput delta
            annotation (Placement(transformation(extent={{98,-92},{118,-70}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-100,20},{-80,40}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-100,-10},{-80,10}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.1,
            offset=0,
            startTime=1)                                    annotation (Placement(
                transformation(extent={{-100,-40},{-80,-20}})));
          Interfaces.SMIB_AVR_PSS_wInput_wLineRmoval
            sMIB_AVR_PSS_wInput_wLineRmoval
            annotation (Placement(transformation(extent={{-20,-18},{8,10}})));
        equation
          connect(PSSchange.y, sMIB_AVR_PSS_wInput_wLineRmoval.uPSS)
            annotation (Line(points={{-79,30},{-52,30},{-52,4},{-22,4}},
                color={0,0,127}));
          connect(Pmchange.y, sMIB_AVR_PSS_wInput_wLineRmoval.uPm) annotation (
              Line(points={{-79,0},{-50,0},{-50,-4},{-22,-4}}, color={0,0,127}));
          connect(Ploadchange.y, sMIB_AVR_PSS_wInput_wLineRmoval.uPload)
            annotation (Line(points={{-79,-30},{-50,-30},{-50,-12},{-22,-12}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.Vt, Vt) annotation (Line(
                points={{9,4},{55.5,4},{55.5,79},{108,79}},     color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.P, P) annotation (Line(points={{9,0},{
                  55.5,0},{55.5,41},{108,41}},            color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.Q, Q) annotation (Line(points={{9,-4},{
                  55.5,-4},{55.5,1},{108,1}},             color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.w, w) annotation (Line(points={{9,-8},{
                  54.5,-8},{54.5,-39},{108,-39}},             color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.delta, delta) annotation (
              Line(points={{9,-12},{56.5,-12},{56.5,-81},{108,-81}},     color=
                  {0,0,127}));
          annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
                coordinateSystem(preserveAspectRatio=false)));
        end NonlinModel_for_NonlinExperiment;

        model LinearModelExperiment "Simulate the linearized model"
          extends Modelica.Icons.Example;
          extends
            SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
          inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
            annotation (Placement(transformation(extent={{72,70},{92,90}})));
          Modelica.Blocks.Routing.Multiplex3 multiplex3_1(n1=1, n2=1)
            annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
          Modelica_LinearSystems2.Controller.StateSpace stateSpace(system(
              A=[0.0,376.99111838914104,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0;
                  -0.17463047017161476,0.0,0.0,0.0,-0.19691539577745257,
                  0.05165091549501419,0.0,0.0,0.0,0.0,0.0,0.0; -0.2572522495368608,
                  0.0,-0.12500000001607756,0.0,-0.2667626383478494,-0.0011575290076623408,
                  0.0,0.0,0.1250000000007022,0.0,0.0,0.0; 0.4001908174874514,
                  0.0,0.0,-0.9999999999145679,0.007949509372810712,-1.490106156924895,
                  0.0,0.0,0.0,0.0,0.0,0.0; -3.3871249684150193,0.0,
                  33.333333329399665,0.0,-36.84567732273118,-0.015240665872904135,
                  0.0,0.0,0.0,0.0,0.0,0.0; 2.275363726567784,0.0,0.0,
                  14.285714284493826,0.04519850164071178,-22.758006377315837,
                  0.0,0.0,0.0,0.0,0.0,0.0; -9.193376853690735,0.0,0.0,0.0,
                  29.09592091305288,33.296008728114764,-66.66666665953481,0.0,
                  0.0,0.0,0.0,0.0; 0.0,0.0,0.0,0.0,0.0,0.0,0.0,-1.0,0.0,0.0,0.0,
                  0.0; 0.0,18999999.99623801,0.0,0.0,0.0,0.0,-1999999.9997860442,
                  10000.00082740371,-10000.000000056176,0.0,0.0,-18999999.99623801;
                  0.0,0.009499999998119004,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.001,
                  0.0,-0.009499999998119004; 0.0,0.009499999998119004,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0,0.0,-0.001,-0.009499999998119004; 0.0,
                  0.7092198580777036,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.7092198580777036],
              B=[0.0,0.0,0.0; 0.0,0.14285715467719018,-0.031538565410071266;
                  0.0,0.0,-0.04074002246667874; 0.0,0.0,0.09771145004222603;
                  0.0,0.0,-0.5364056421264252; 0.0,0.0,0.5555579805717668; 0.0,
                  0.0,-1.0575688473106009; 0.0,0.0,0.0; 18999998.019353367,0.0,
                  0.0; 0.009499999009676685,0.0,0.0; 0.009499999009676685,0.0,
                  0.0; 0.7092199168371426,0.0,0.0],
              C=[-0.137900652805361,0.0,0.0,0.0,0.4364388136957932,
                  0.4994401309217214,0.0,0.0,0.0,0.0,0.0,0.0;
                  0.3122089845885527,0.0,0.0,0.0,1.4657741560532285,
                  0.770607648801796,0.0,0.0,0.0,0.0,0.0,0.0; 1.2147747707901193,
                  0.0,0.0,0.0,1.371465982478547,-0.35800200991926284,0.0,0.0,
                  0.0,0.0,0.0,0.0; 0.0,0.999999999889562,0.0,0.0,0.0,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0; 0.9999999998632589,0.0,0.0,0.0,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0,0.0],
              D=[0.0,0.0,-0.015863532709659012; 0.0,0.0,0.02177497071542689;
                  0.0,0.0,0.21947776929209795; 0.0,0.0,0.0; 0.0,0.0,0.0],
              yNames={"Vt","Q","P","w","delta"},
              xNames={"sMIB_AVR_PSS_wInput_wFault.G1.machine.delta",
                  "sMIB_AVR_PSS_wInput_wFault.G1.machine.w",
                  "sMIB_AVR_PSS_wInput_wFault.G1.machine.e1q",
                  "sMIB_AVR_PSS_wInput_wFault.G1.machine.e1d",
                  "sMIB_AVR_PSS_wInput_wFault.G1.machine.e2q",
                  "sMIB_AVR_PSS_wInput_wFault.G1.machine.e2d",
                  "sMIB_AVR_PSS_wInput_wFault.G1.avr.vm",
                  "sMIB_AVR_PSS_wInput_wFault.G1.avr.vr",
                  "sMIB_AVR_PSS_wInput_wFault.G1.avr.vf1",
                  "sMIB_AVR_PSS_wInput_wFault.G1.pss.imLeadLag.TF.x_scaled[1]",
                  "sMIB_AVR_PSS_wInput_wFault.G1.pss.imLeadLag1.TF.x_scaled[1]",
                  "sMIB_AVR_PSS_wInput_wFault.G1.pss.derivativeLag.TF.x_scaled[1]"},
              uNames={"uPSS","uPm","uPload"}),
                                      blockType=Modelica_LinearSystems2.Controller.Types.BlockTypeWithGlobalDefault.Continuous)
            annotation (Placement(transformation(extent={{-36,-10},{-16,10}})));

          Modelica.Blocks.Routing.DeMultiplex5 demultiplex2_2
            annotation (Placement(transformation(extent={{60,-20},{100,20}})));
          Modelica.Blocks.Math.Add addy[5]
            annotation (Placement(transformation(extent={{6,-16},{26,4}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-124,-10},{-104,10}})));
          Modelica.Blocks.Routing.Multiplex5 multiplex5_1 annotation (Placement(
                transformation(
                extent={{-14,-14},{14,14}},
                rotation=90,
                origin={2,-38})));
          Modelica.Blocks.Sources.Constant y0_Vt(k=1) annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={-80,-90})));
          Modelica.Blocks.Sources.Constant y0_Q(k=0.200359) annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={-40,-90})));
          Modelica.Blocks.Sources.Constant y0_P(k=0.899999)
                                                       annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={0,-90})));
          Modelica.Blocks.Sources.Constant y0_w(k=1) annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={40,-90})));
          Modelica.Blocks.Sources.Constant y0_delta(k=1.29897) annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={80,-90})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.1,
            offset=0,
            startTime=1)                                    annotation (Placement(
                transformation(extent={{-124,-42},{-104,-22}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-122,20},{-102,40}})));
        equation
          connect(multiplex3_1.y, stateSpace.u)
            annotation (Line(points={{-59,0},{-38,0}}, color={0,0,127}));
          connect(demultiplex2_2.y1[1], Vt) annotation (Line(points={{102,16},{108,16},{
                  108,79},{150,79}},
                                 color={0,0,127}));
          connect(demultiplex2_2.y4[1], w) annotation (Line(points={{102,-8},{110,-8},{110,
                  -39},{150,-39}}, color={0,0,127}));
          connect(demultiplex2_2.y5[1], delta) annotation (Line(points={{102,-16},{108,-16},
                  {108,-81},{150,-81}}, color={0,0,127}));
          connect(addy.y, demultiplex2_2.u)
            annotation (Line(points={{27,-6},{28,-6},{28,0},{56,0}}, color={0,0,127}));
          connect(stateSpace.y, addy.u1)
            annotation (Line(points={{-15,0},{4,0}}, color={0,0,127}));
          connect(Pmchange.y,multiplex3_1. u2[1]) annotation (Line(points={{-103,0},
                  {-82,0}},                color={0,0,127}));
          connect(multiplex5_1.y, addy.u2)
            annotation (Line(points={{2,-22.6},{2,-12},{4,-12}}, color={0,0,127}));
          connect(multiplex5_1.u1[1], y0_Vt.y) annotation (Line(points={{-12,-54.8},{-80,
                  -54.8},{-80,-79}}, color={0,0,127}));
          connect(y0_Q.y, multiplex5_1.u2[1]) annotation (Line(points={{-40,-79},{-40,-64},
                  {-5,-64},{-5,-54.8}}, color={0,0,127}));
          connect(y0_P.y, multiplex5_1.u3[1])
            annotation (Line(points={{0,-79},{2,-79},{2,-54.8}}, color={0,0,127}));
          connect(y0_w.y, multiplex5_1.u4[1]) annotation (Line(points={{40,-79},{40,-64},
                  {9,-64},{9,-54.8}}, color={0,0,127}));
          connect(y0_delta.y, multiplex5_1.u5[1]) annotation (Line(points={{80,-79},{80,
                  -54.8},{16,-54.8}}, color={0,0,127}));
          connect(demultiplex2_2.y3[1], P) annotation (Line(points={{102,0},{
                  122,0},{122,41},{150,41}}, color={0,0,127}));
          connect(Q, demultiplex2_2.y2[1]) annotation (Line(points={{150,1},{
                  126,1},{126,8},{102,8}}, color={0,0,127}));
          connect(Ploadchange.y, multiplex3_1.u3[1]) annotation (Line(points={{-103,
                  -32},{-96,-32},{-96,-7},{-82,-7}},      color={0,0,127}));
          connect(PSSchange.y, multiplex3_1.u1[1]) annotation (Line(points={{
                  -101,30},{-92,30},{-92,7},{-82,7}}, color={0,0,127}));
          annotation (
            Icon(coordinateSystem(preserveAspectRatio=false)),
            Diagram(coordinateSystem(preserveAspectRatio=false), graphics={
                Text(
                  extent={{-58,24},{-44,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="du",
                  fontSize=24),
                Text(
                  extent={{-38,-18},{-22,-38}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="y0",
                  fontSize=24),
                Text(
                  extent={{40,20},{50,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="y"),
                Text(
                  extent={{-12,22},{2,6}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="dy"),
                Text(
                  extent={{-80,80},{0,60}},
                  lineColor={85,170,255},
                  fillPattern=FillPattern.HorizontalCylinder,
                  fillColor={28,108,200},
                  horizontalAlignment=TextAlignment.Left,
                  textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
                Line(
                  points={{-4,64},{24,44},{18,14}},
                  color={0,0,255},
                  thickness=0.5,
                  arrow={Arrow.None,Arrow.Filled},
                  smooth=Smooth.Bezier)}),
            experiment(
              StopTime=20,
              __Dymola_NumberOfIntervals=10000,
              Tolerance=1e-06,
              __Dymola_fixedstepsize=0.01,
              __Dymola_Algorithm="Dassl"),
            __Dymola_Commands(file="MosScripts/linearize_and_compare.mos"
                "linearize_and_compare", file=
                  "MosScripts/linearize_and_compare.mos"
                "linearize_and_compare"));
        end LinearModelExperiment;

        function LinearizeSimple
          // Import things needed for the calculations
          import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
          // Declare outputs to display
          output Real A[:,:] "A-matrix";
          output Real B[:,:] "B-matrix";
          output Real C[:,:] "C-matrix";
          output Real D[:,:] "D-matrix";
          output String inputNames[:] "Modelica names of inputs";
          output String outputNames[:] "Modelica names of outputs";
          output String stateNames[:] "Modelica names of states";
          // Declare reconfigurable inputs
          input Modelica.SIunits.Time tlin = 20 "t for model linearization";
          input Modelica.SIunits.Time tsim = 20 "Simulation time";
          input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAtZero.NonlinModel_for_Linearization";
          input String pathToNonlinearExperiment = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAtZero.NonlinModel_for_NonlinExperiment";
        algorithm
          // Compute and display the ABCD matrices, etc
          Modelica.Utilities.Streams.print("Linearized Model");
          (A,B,C,D,inputNames,outputNames,stateNames) :=
            Modelica_LinearSystems2.Utilities.Import.linearize(
            pathToNonlinearPlantModel,tlin);
          nx := size(A, 1); //number of states
          Modelica.Utilities.Streams.print("Number of states" + String(nx));
          // Now we want to extract the initial value of the outputs to use it in our
          // linear model response
          Modelica.Utilities.Streams.print("Simulating nonlinear model");
          simulateModel(
            pathToNonlinearExperiment,
            stopTime=tsim,
            numberOfIntervals=1000,
            resultFile="res_nl");
           ny := size(C, 1);
           y0 := DymolaCommands.Trajectories.readTrajectory(
             "res_nl.mat",
             {outputNames[i] for i in 1:ny},
             DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat"));
           DataFiles.writeMATmatrix(
             "MyData.mat",
             "y0",
             [y0[1:ny,1]],
             append=true);
          // Print y0's first values which is needed for the linear response model
          y0out := y0[:,1]; // we only want the first few elements
          Modelica.Utilities.Streams.print("y0 =");
          Modelica.Math.Vectors.toString(y0out);
          annotation(__Dymola_interactive=true);
        end LinearizeSimple;

        function LinearizeAndCompare
          "Linearizes the model at initialization and after a disturbance at a given time"
          // See the Documentation for an explanation of the goals.
          // IMPORTING FUNCTIONS
          // Import things needed for the calculations
          import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
          // OUTPUTS OF THEFUNCTION - FOR DISPLAY
          // Declare outputs to display
          output Real A[:,:] "A-matrix";
          output Real B[:,:] "B-matrix";
          output Real C[:,:] "C-matrix";
          output Real D[:,:] "D-matrix";
          output String inputNames[:] "Modelica names of inputs";
          output String outputNames[:] "Modelica names of outputs";
          output String stateNames[:] "Modelica names of states";
          output Real y0out[:] "Initial value of the output variables";
          // INPUTS TO THE FUNCTION
          // Declare reconfigurable simulation parameters
          input Modelica.SIunits.Time tlin = 0 "t for model linearization";
          input Modelica.SIunits.Time tsim = 20 "Simulation time";
          input Real numberOfIntervalsin=10000 "No. of intervals";
          // Use this for Case A
          //input String method = "Rkfix4" "Solver";
          input String methodin = "DASSL" "Solver";
          input Real fixedstepsizein= 1e-6 "Time step - needed only for fixed time step solvers";
          //
          // DEFINING THE NONLINEAR PLANT, NONLINEAR EXPERIMENT, AND LINEAR EXPERIMENT MODELS
          //
          // 1) NONLINEAR PLANT:
          // This is the model that will be linearized, i.e. the nonlinear plant model
           input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAtZero.NonlinModel_for_Linearization" "Nonlinear plant model";
          //
          //
          // 2) NONLINEAR EXPERIMENT: this is a model which applies a change to the input of the nonlinear model.
          // It must match the nonlinar plant above.
          // This model will be simulated, and the simulation results will be compared to the simulation of the corresponding linearized model.
          input String pathToNonlinearExperiment= "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAtZero.NonlinModel_for_NonlinExperiment" "Nonlinear experiment model";
          //
          //
          // 3) LINEAR EXPERIMENT: this is a template that can be used for all three cases, so it is not necessary to create other cases here
          input String pathToLinearExperiment = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAtZero.LinearModelGeneral";

        algorithm
          // Compute and display the ABCD matrices, etc
          (A,B,C,D,inputNames,outputNames,stateNames) :=
            Modelica_LinearSystems2.Utilities.Import.linearize(
            pathToNonlinearPlantModel,tlin);
          // LINEARIZE plant model at t_lin
          // This is the same as above, however, it stores it in a StateSpace object
          ss := Modelica_LinearSystems2.ModelAnalysis.Linearize(
            pathToNonlinearPlantModel, simulationSetup=
            Modelica_LinearSystems2.Records.SimulationOptionsForLinearization(
            linearizeAtInitial=false, t_linearize=tlin));
          // PRINT linear system
          Modelica.Utilities.Streams.print(String(ss));
          // SAVE the data in a mat file
          DataFiles.writeMATmatrix(
            "MyData.mat",
            "ABCD",
            [ss.A, ss.B; ss.C, ss.D],
            append=false);
          nx := size(ss.A, 1);
          DataFiles.writeMATmatrix(
            "MyData.mat",
            "nx",
            [nx],
            append=true);
          Modelica.Utilities.Streams.print("Simulating nonlinear model");
          simulateModel(
            pathToNonlinearExperiment,
            stopTime=tsim,
            numberOfIntervals=numberOfIntervalsin, method = methodin, fixedstepsize=fixedstepsizein,
            resultFile="res_nl");
           ny := size(ss.C, 1);
           y0 := DymolaCommands.Trajectories.readTrajectory(
             "res_nl.mat",
             {ss.yNames[i] for i in 1:ny},
             DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat"));
            // {"PS_ConstantEfd." + ss.yNames[i] for i in 1:ny},
           DataFiles.writeMATmatrix(
             "MyData.mat",
             "y0",
             [y0[1:ny,1]],
             append=true);
          // Print y0's first values which is needed for the linear response model
          y0out := y0[:,1]; // we only want the first few elements
          Modelica.Utilities.Streams.print("y0 =");
          Modelica.Math.Vectors.toString(y0out);
          //
          // We now simulate the linear model, which requires y0
          Modelica.Utilities.Streams.print("Simulating linear model");
          simulateModel(
            pathToLinearExperiment,
            stopTime=tsim,
            numberOfIntervals=numberOfIntervalsin, method = methodin, fixedstepsize=fixedstepsizein,
            resultFile="res_lin");
            // Plot
        removePlots(true);
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Vt"}, range={0.0, 20.0, 0.998, 1.002}, grid=true, filename="res_nl.mat", colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Q"}, range={0.0, 20.0, 0.18, 0.21}, grid=true, subPlot=102, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"P"}, range={0.0, 20.0, 0.86, 0.94}, grid=true, subPlot=103, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"w"}, range={0.0, 20.0, 0.9996, 1.0004}, grid=true, subPlot=104, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"delta"}, range={0.0, 20.0, 1.27, 1.3}, grid=true, subPlot=105, colors={{28,108,200}}, displayUnits={"rad"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Vt"}, range={0.0, 20.0, 0.998, 1.002}, erase=false, grid=true, filename="res_lin.mat", colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Q"}, range={0.0, 20.0, 0.18, 0.21}, erase=false, grid=true, subPlot=102, colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"P"}, range={0.0, 20.0, 0.86, 0.94}, erase=false, grid=true, subPlot=103, colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"w"}, range={0.0, 20.0, 0.9996, 1.0004}, erase=false, grid=true, subPlot=104, colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"delta"}, range={0.0, 20.0, 1.27, 1.3}, erase=false, grid=true, subPlot=105, colors={{238,46,47}});


          annotation(__Dymola_interactive=true, Documentation(info="<html>
<p>This function linearizes the model at two different times, initialization and at a user provided time.</p>
</html>"));
        end LinearizeAndCompare;

        model LinearModelGeneral
          "Simulate the linearized SMIB model obtained by running the function LinearizeSMIB."
          extends SMIBPS_IdControl.Utilities.Icons.FunctionDependentExample;
          extends
            SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
          // The following definitions are very important to couple the linear model
          // to the linearization of the nonlinear model and the simulation
          parameter Real[:] y0=vector(DataFiles.readMATmatrix("MyData.mat", "y0")) annotation (Evaluate=false);
          // The following has to be imported in order to be able to interpret and manipulate the StateSpace types
          import Modelica_LinearSystems2.StateSpace;
          parameter StateSpace ss=StateSpace.Import.fromFile("MyData.mat", "ABCD");
          parameter Integer ny=size(ss.C, 1);
          inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
            annotation (Placement(transformation(extent={{60,60},{80,80}})));
          Modelica.Blocks.Routing.Multiplex3 multiplex3_1(n1=1, n2=1)
            annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
          Modelica.Blocks.Routing.DeMultiplex5 demultiplex2_2
            annotation (Placement(transformation(extent={{60,-20},{100,20}})));
          Modelica.Blocks.Math.Add addy[ny]
            annotation (Placement(transformation(extent={{6,-16},{26,4}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-122,-10},{-102,10}})));
          Modelica.Blocks.Sources.Constant y0_initial[ny](k=y0)      annotation (
              Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={0,-32})));
          Modelica_LinearSystems2.Controller.StateSpace stateSpace(system=ss)
            annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-120,20},{-100,40}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.1,
            offset=0,
            startTime=1)                                    annotation (Placement(
                transformation(extent={{-120,-40},{-100,-20}})));
        equation
          connect(demultiplex2_2.y1[1], Vt) annotation (Line(points={{102,16},{108,16},{
                  108,79},{150,79}},
                                 color={0,0,127}));
          connect(demultiplex2_2.y4[1], w) annotation (Line(points={{102,-8},{110,-8},{110,
                  -39},{150,-39}}, color={0,0,127}));
          connect(demultiplex2_2.y5[1], delta) annotation (Line(points={{102,-16},{108,-16},
                  {108,-81},{150,-81}}, color={0,0,127}));
          connect(addy.y, demultiplex2_2.u)
            annotation (Line(points={{27,-6},{28,-6},{28,0},{56,0}}, color={0,0,127}));
          connect(Pmchange.y,multiplex3_1. u2[1]) annotation (Line(points={{-101,0},
                  {-82,0}},                color={0,0,127}));
          connect(demultiplex2_2.y3[1], P) annotation (Line(points={{102,0},{122,0},{122,
                  41},{150,41}}, color={0,0,127}));
          connect(Q, demultiplex2_2.y2[1]) annotation (Line(points={{150,1},{126,1},{126,
                  8},{102,8}}, color={0,0,127}));
          connect(y0_initial.y, addy.u2)
            annotation (Line(points={{0,-21},{0,-12},{4,-12}}, color={0,0,127}));
          connect(multiplex3_1.y, stateSpace.u)
            annotation (Line(points={{-59,0},{-42,0}}, color={0,0,127}));
          connect(stateSpace.y, addy.u1)
            annotation (Line(points={{-19,0},{4,0}}, color={0,0,127}));
          connect(PSSchange.y, multiplex3_1.u1[1]) annotation (Line(points={{
                  -99,30},{-92,30},{-92,7},{-82,7}}, color={0,0,127}));
          connect(Ploadchange.y, multiplex3_1.u3[1]) annotation (Line(points={{
                  -99,-30},{-90,-30},{-90,-7},{-82,-7}}, color={0,0,127}));
          annotation (
            Icon(coordinateSystem(preserveAspectRatio=false)),
            Diagram(coordinateSystem(preserveAspectRatio=false), graphics={
                Text(
                  extent={{-58,24},{-44,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="du",
                  fontSize=24),
                Text(
                  extent={{-12,20},{-2,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="dy",
                  fontSize=24),
                Text(
                  extent={{-38,-18},{-22,-38}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="y0",
                  fontSize=24),
                Text(
                  extent={{40,20},{50,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="y"),
                Text(
                  extent={{-80,80},{0,60}},
                  lineColor={85,170,255},
                  fillPattern=FillPattern.HorizontalCylinder,
                  fillColor={28,108,200},
                  horizontalAlignment=TextAlignment.Left,
                  textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
                Text(
                  extent={{-80,-80},{80,-100}},
                  lineColor={85,170,255},
                  fillPattern=FillPattern.HorizontalCylinder,
                  fillColor={28,108,200},
                  horizontalAlignment=TextAlignment.Left,
                  textString="Notice the change in the order of the outputs w.r.t. the nonlinear model.
They have to be rearranged based on the order provided by the linearization function."),
                Line(
                  points={{66,-86},{106,-84},{120,-4}},
                  color={0,0,255},
                  thickness=0.5,
                  arrow={Arrow.None,Arrow.Filled},
                  smooth=Smooth.Bezier),
                Line(
                  points={{-4,68},{24,48},{18,18}},
                  color={0,0,255},
                  thickness=0.5,
                  arrow={Arrow.None,Arrow.Filled},
                  smooth=Smooth.Bezier)}),
            experiment(
              StopTime=15,
              __Dymola_NumberOfIntervals=1000,
              __Dymola_Algorithm="Dassl"),
            Documentation(info="<html>
<p>DO NOT try to run this model on it&apos;s own! </p>
<p>Models with this icon will not simulate on their own, instead they work together with a function that populates certain parameters in the model and perform other operations.</p>
<p><br>See the associated function to run:<span style=\"color: #1c6cc8;\"> <span style=\"font-family: Courier New; font-size: 8pt;\">LinearizeSMIBGeneral </span>within <span style=\"font-family: Courier New; font-size: 8pt; color: #1c6cc8;\">SMIB_PSControl.Analysis.LinearAnalysis.BasicLinearization.LinearizeSMIBGeneral()</span></p>
</html>"));
        end LinearModelGeneral;
      end LinAtZero;

      package LinAfterDisturbance
        extends Modelica.Icons.ExamplesPackage;
        model NonlinModel_for_Linearization
          extends Modelica.Icons.Example;
          Modelica.Blocks.Interfaces.RealOutput Vt
            annotation (Placement(transformation(extent={{100,70},{120,90}}),
                iconTransformation(extent={{100,70},{120,90}})));
        public
          Modelica.Blocks.Interfaces.RealOutput Q
            annotation (Placement(transformation(extent={{100,-10},{120,10}}),
                iconTransformation(extent={{100,-10},{120,10}})));
          Modelica.Blocks.Interfaces.RealOutput P
            annotation (Placement(transformation(extent={{100,30},{120,50}}),
                iconTransformation(extent={{100,30},{120,50}})));
          Modelica.Blocks.Interfaces.RealOutput w
            annotation (Placement(transformation(extent={{100,-50},{120,-30}}),
                iconTransformation(extent={{100,-50},{120,-30}})));
          Modelica.Blocks.Interfaces.RealOutput delta
            annotation (Placement(transformation(extent={{100,-90},{120,-70}}),
                iconTransformation(extent={{100,-90},{120,-70}})));
          Modelica.Blocks.Interfaces.RealInput uPSS
            annotation (Placement(transformation(extent={{-140,40},{-100,80}})));
          Modelica.Blocks.Interfaces.RealInput uPm
            annotation (Placement(transformation(extent={{-140,-20},{-100,20}})));
          Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
                transformation(extent={{-140,-80},{-100,-40}})));
          Interfaces.SMIB_AVR_PSS_wInput_wLineRmoval
            sMIB_AVR_PSS_wInput_wLineRmoval(t1=0.5)
            annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
        equation
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPSS, uPSS) annotation (Line(
                points={{-45.7143,22.8571},{-79.4286,22.8571},{-79.4286,60},{
                  -120,60}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPm, uPm)
            annotation (Line(points={{-45.7143,0},{-120,0}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPload, uPload) annotation (
              Line(points={{-45.7143,-22.8571},{-78.8572,-22.8571},{-78.8572,
                  -60},{-120,-60}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.Vt, Vt) annotation (Line(
                points={{42.8571,22.8571},{72.4286,22.8571},{72.4286,80},{110,
                  80}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.P, P) annotation (Line(points={{42.8571,
                  11.4286},{86,11.4286},{86,40},{110,40}},          color={0,0,
                  127}));
          connect(Q, sMIB_AVR_PSS_wInput_wLineRmoval.Q) annotation (Line(points={{110,0},
                  {76,0},{76,0},{42.8571,0}},                       color={0,0,
                  127}));
          connect(w, sMIB_AVR_PSS_wInput_wLineRmoval.w) annotation (Line(points={{110,-40},
                  {84,-40},{84,-11.4286},{42.8571,-11.4286}},           color={
                  0,0,127}));
          connect(delta, sMIB_AVR_PSS_wInput_wLineRmoval.delta) annotation (
              Line(points={{110,-80},{68,-80},{68,-22.8571},{42.8571,-22.8571}},
                color={0,0,127}));
          annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
                coordinateSystem(preserveAspectRatio=false)));
        end NonlinModel_for_Linearization;

        model NonlinModel_for_NonlinExperiment
          extends Modelica.Icons.Example;
          Modelica.Blocks.Interfaces.RealOutput Vt
            annotation (Placement(transformation(extent={{100,70},{120,90}}),
                iconTransformation(extent={{100,70},{120,90}})));
        public
          Modelica.Blocks.Interfaces.RealOutput Q
            annotation (Placement(transformation(extent={{100,-10},{120,10}}),
                iconTransformation(extent={{100,-10},{120,10}})));
          Modelica.Blocks.Interfaces.RealOutput P
            annotation (Placement(transformation(extent={{100,30},{120,50}}),
                iconTransformation(extent={{100,30},{120,50}})));
          Modelica.Blocks.Interfaces.RealOutput w
            annotation (Placement(transformation(extent={{100,-50},{120,-30}}),
                iconTransformation(extent={{100,-50},{120,-30}})));
          Modelica.Blocks.Interfaces.RealOutput delta
            annotation (Placement(transformation(extent={{100,-90},{120,-70}}),
                iconTransformation(extent={{100,-90},{120,-70}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-100,20},{-80,40}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-100,-10},{-80,10}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.05,
            offset=0,
            startTime=30.5)                                 annotation (Placement(
                transformation(extent={{-100,-42},{-80,-22}})));
          Interfaces.SMIB_AVR_PSS_wInput_wLineRmoval
            sMIB_AVR_PSS_wInput_wLineRmoval(t1=0.5)
            annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
        equation
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPSS, PSSchange.y)
            annotation (Line(points={{-45.7143,22.8571},{-62.4286,22.8571},{
                  -62.4286,30},{-79,30}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPm, Pmchange.y)
            annotation (Line(points={{-45.7143,0},{-79,0}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.uPload, Ploadchange.y)
            annotation (Line(points={{-45.7143,-22.8571},{-62.8572,-22.8571},{
                  -62.8572,-32},{-79,-32}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmoval.Vt, Vt) annotation (Line(
                points={{42.8571,22.8571},{42.8571,24},{64,24},{64,80},{110,80}},
                color={0,0,127}));
          connect(P, sMIB_AVR_PSS_wInput_wLineRmoval.P) annotation (Line(points={{110,40},
                  {68,40},{68,11.4286},{42.8571,11.4286}},          color={0,0,
                  127}));
          connect(Q, sMIB_AVR_PSS_wInput_wLineRmoval.Q) annotation (Line(points={{110,0},
                  {76,0},{76,0},{42.8571,0}},                       color={0,0,
                  127}));
          connect(w, sMIB_AVR_PSS_wInput_wLineRmoval.w) annotation (Line(points={{110,-40},
                  {80,-40},{80,-11.4286},{42.8571,-11.4286}},           color={
                  0,0,127}));
          connect(delta, sMIB_AVR_PSS_wInput_wLineRmoval.delta) annotation (
              Line(points={{110,-80},{60,-80},{60,-22.8571},{42.8571,-22.8571}},
                color={0,0,127}));
          annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
                coordinateSystem(preserveAspectRatio=false)),
            experiment(
              StopTime=40,
              __Dymola_NumberOfIntervals=1000,
              __Dymola_Algorithm="Dassl"));
        end NonlinModel_for_NonlinExperiment;

        model LinearModelExperiment "Simulate the linearized model"
          extends Modelica.Icons.Example;
          extends
            SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
          Modelica.Blocks.Sources.Constant
                                       const(k=0)
            annotation (Placement(transformation(extent={{-124,22},{-104,42}})));
          inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
            annotation (Placement(transformation(extent={{72,70},{92,90}})));
          Modelica.Blocks.Routing.Multiplex3 multiplex3_1(n1=1, n2=1)
            annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
          Modelica_LinearSystems2.Controller.StateSpace stateSpace(system(
              A=[0.0,376.99111838900376,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0;
                  -0.06866172157436483,0.0,0.0,0.0,-0.13089553805695955,-0.007996158813506517,
                  0.0,0.0,0.0,0.0,0.0,0.0; -0.17084367956584362,0.0,-0.12499999999370072,
                  0.0,-0.17040348889278564,-0.0005205012335237335,0.0,0.0,
                  0.12500000001264727,0.0,0.0,0.0; -0.054929009666219,0.0,0.0,-1.0000000000918718,
                  0.006479547985583815,-0.9628997952694285,0.0,0.0,0.0,0.0,0.0,
                  0.0; -2.2494220893758543,0.0,33.33333333165353,0.0,-35.57695963015827,
                  -0.0068532061489849395,0.0,0.0,0.0,0.0,0.0,0.0; -0.31230970471005864,
                  0.0,0.0,14.28571428702674,0.0368407467393937,-19.76047075622232,
                  0.0,0.0,0.0,0.0,0.0,0.0; -11.01505344351068,0.0,0.0,0.0,
                  39.131006300072514,34.81062225999472,-66.6666666604358,0.0,
                  0.0,0.0,0.0,0.0; 0.0,0.0,0.0,0.0,0.0,0.0,0.0,-1.0,0.0,0.0,0.0,
                  0.0; 0.0,18999999.99623109,0.0,0.0,0.0,0.0,-1999999.9998130735,
                  10000.00082740371,-10000.000001011782,0.0,0.0,-18999999.99978613;
                  0.0,0.009499999998115545,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.001,
                  0.0,-0.009499999999893066; 0.0,0.009499999998115545,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0,0.0,-0.001,-0.009499999999893066; 0.0,
                  0.7092198580774454,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.7092198580776647],
              B=[0.0,0.0,0.0; 0.0,0.14285715467719584,-0.04316848465992149; 0.0,
                  0.0,-0.06524164541943378; 0.0,0.0,0.11073453265453281; 0.0,
                  0.0,-0.8590077723210262; 0.0,0.0,0.6296031156744724; 0.0,0.0,
                  -2.8629173106272297; 0.0,0.0,0.0; 19000001.57206705,0.0,0.0;
                  0.009500000786033526,0.0,0.0; 0.009500000786033526,0.0,0.0;
                  0.7092199168371426,0.0,0.0],
              C=[-0.1652258016526602,0.0,0.0,0.0,0.5869650945010877,
                  0.5221593338999209,0.0,0.0,0.0,0.0,0.0,0.0;
                  0.5205641319797044,0.0,0.0,0.0,1.3973765860221798,
                  0.30371677737740654,0.0,0.0,0.0,0.0,0.0,0.0;
                  0.4757286783602097,0.0,0.0,0.0,0.9112481248640445,
                  0.057999429135694204,0.0,0.0,0.0,0.0,0.0,0.0; 0.0,
                  0.999999999889198,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0;
                  0.9999999998823222,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                  0.0],
              D=[0.0,0.0,-0.04294375965940844; 0.0,0.0,0.10523959481645306; 0.0,
                  0.0,0.300027558353122; 0.0,0.0,0.0; 0.0,0.0,0.0],
              yNames={"Vt","Q","P","w","delta"},
              xNames={"sMIB_AVR_PSS_wInput_wFault.G1.machine.delta",
                  "sMIB_AVR_PSS_wInput_wFault.G1.machine.w",
                  "sMIB_AVR_PSS_wInput_wFault.G1.machine.e1q",
                  "sMIB_AVR_PSS_wInput_wFault.G1.machine.e1d",
                  "sMIB_AVR_PSS_wInput_wFault.G1.machine.e2q",
                  "sMIB_AVR_PSS_wInput_wFault.G1.machine.e2d",
                  "sMIB_AVR_PSS_wInput_wFault.G1.avr.vm",
                  "sMIB_AVR_PSS_wInput_wFault.G1.avr.vr",
                  "sMIB_AVR_PSS_wInput_wFault.G1.avr.vf1",
                  "sMIB_AVR_PSS_wInput_wFault.G1.pss.imLeadLag.TF.x_scaled[1]",
                  "sMIB_AVR_PSS_wInput_wFault.G1.pss.imLeadLag1.TF.x_scaled[1]",
                  "sMIB_AVR_PSS_wInput_wFault.G1.pss.derivativeLag.TF.x_scaled[1]"},
              uNames={"uPSS","uPm","uPload"}),
                                      blockType=Modelica_LinearSystems2.Controller.Types.BlockTypeWithGlobalDefault.Continuous)
            annotation (Placement(transformation(extent={{-36,-10},{-16,10}})));

          Modelica.Blocks.Routing.DeMultiplex5 demultiplex2_2
            annotation (Placement(transformation(extent={{60,-20},{100,20}})));
          Modelica.Blocks.Math.Add addy[5]
            annotation (Placement(transformation(extent={{6,-16},{26,4}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-124,-10},{-104,10}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.05,
            offset=0,
            startTime=30.5)                                 annotation (Placement(
                transformation(extent={{-124,-42},{-104,-22}})));
          Modelica.Blocks.Sources.Constant y0_initial[5](k={0.998570621013641,
                0.427906930446625,0.89956396818161,0.999999940395355,
                1.62256383895874})
            annotation (Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={0,-52})));
        equation
          connect(multiplex3_1.u1[1], const.y) annotation (Line(points={{-82,7},{
                  -90,7},{-90,32},{-103,32}}, color={0,0,127}));
          connect(multiplex3_1.y, stateSpace.u)
            annotation (Line(points={{-59,0},{-38,0}}, color={0,0,127}));
          connect(demultiplex2_2.y1[1], Vt) annotation (Line(points={{102,16},{
                  108,16},{108,80},{150,80}},
                                 color={0,0,127}));
          connect(demultiplex2_2.y4[1], w) annotation (Line(points={{102,-8},{
                  110,-8},{110,-40},{150,-40}},
                                   color={0,0,127}));
          connect(demultiplex2_2.y5[1], delta) annotation (Line(points={{102,-16},
                  {108,-16},{108,-80},{150,-80}},
                                        color={0,0,127}));
          connect(addy.y, demultiplex2_2.u)
            annotation (Line(points={{27,-6},{28,-6},{28,0},{56,0}}, color={0,0,127}));
          connect(stateSpace.y, addy.u1)
            annotation (Line(points={{-15,0},{4,0}}, color={0,0,127}));
          connect(Pmchange.y,multiplex3_1. u2[1]) annotation (Line(points={{-103,0},
                  {-82,0}},                color={0,0,127}));
          connect(demultiplex2_2.y3[1], P) annotation (Line(points={{102,0},{
                  122,0},{122,40},{150,40}}, color={0,0,127}));
          connect(Q, demultiplex2_2.y2[1]) annotation (Line(points={{150,0},{
                  126,0},{126,8},{102,8}}, color={0,0,127}));
          connect(Ploadchange.y, multiplex3_1.u3[1]) annotation (Line(points={{-103,
                  -32},{-96,-32},{-96,-7},{-82,-7}},      color={0,0,127}));
          connect(y0_initial.y, addy.u2) annotation (Line(points={{0,-41},{0,
                  -12},{4,-12}}, color={0,0,127}));
          annotation (
            Icon(coordinateSystem(preserveAspectRatio=false)),
            Diagram(coordinateSystem(preserveAspectRatio=false), graphics={
                Text(
                  extent={{-58,24},{-44,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="du",
                  fontSize=24),
                Text(
                  extent={{-38,-18},{-22,-38}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="y0",
                  fontSize=24),
                Text(
                  extent={{40,20},{50,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="y"),
                Text(
                  extent={{-12,22},{2,6}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="dy"),
                Text(
                  extent={{-80,80},{0,60}},
                  lineColor={85,170,255},
                  fillPattern=FillPattern.HorizontalCylinder,
                  fillColor={28,108,200},
                  horizontalAlignment=TextAlignment.Left,
                  textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
                Line(
                  points={{-4,64},{24,44},{18,14}},
                  color={0,0,255},
                  thickness=0.5,
                  arrow={Arrow.None,Arrow.Filled},
                  smooth=Smooth.Bezier)}),
            experiment(
              StopTime=30,
              __Dymola_NumberOfIntervals=10000,
              Tolerance=1e-06,
              __Dymola_fixedstepsize=0.01,
              __Dymola_Algorithm="Dassl"));
        end LinearModelExperiment;

        function LinearizeSimple
          // Import things needed for the calculations
          import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
          // Declare outputs to display
          output Real A[:,:] "A-matrix";
          output Real B[:,:] "B-matrix";
          output Real C[:,:] "C-matrix";
          output Real D[:,:] "D-matrix";
          output String inputNames[:] "Modelica names of inputs";
          output String outputNames[:] "Modelica names of outputs";
          output String stateNames[:] "Modelica names of states";
          // Declare reconfigurable inputs
          input Modelica.SIunits.Time tlin = 30 "t for model linearization";
          input Modelica.SIunits.Time tsim = 30 "Simulation time";
          input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAfterDisturbance.NonlinModel_for_Linearization";
          input String pathToNonlinearExperiment = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAfterDisturbance.NonlinModel_for_NonlinExperiment";
        algorithm
          // Compute and display the ABCD matrices, etc
          Modelica.Utilities.Streams.print("Linearized Model");
          (A,B,C,D,inputNames,outputNames,stateNames) :=
            Modelica_LinearSystems2.Utilities.Import.linearize(
            pathToNonlinearPlantModel,tlin);
          nx := size(A, 1); //number of states
          Modelica.Utilities.Streams.print("Number of states: " + String(nx));
          // Now we want to extract the initial value of the outputs to use it in our
          // linear model response
          Modelica.Utilities.Streams.print("Simulating nonlinear model");
          simulateModel(
            pathToNonlinearExperiment,
            stopTime=tsim,
            numberOfIntervals=1000, tolerance = 1e-6,
            resultFile="res_nl");
           ylen :=DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat");
           ny := size(C, 1);
           y0 := DymolaCommands.Trajectories.readTrajectory(
             "res_nl.mat",
             {outputNames[i] for i in 1:ny},
             DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat"));
           DataFiles.writeMATmatrix(
             "MyData.mat",
             "y0",
             [y0[1:ny,ylen]],
             append=true);
          // Print y0's last values which is needed for the linear response model
          y0out := y0[:,ylen]; // we only want the last few elements
          Modelica.Utilities.Streams.print("y0 = ");
          Modelica.Math.Vectors.toString(y0out);
          annotation(__Dymola_interactive=true);
        end LinearizeSimple;

        function Compare "Compares the responses"
          // INPUTS TO THE FUNCTION
          // Declare reconfigurable simulation parameters
          input Modelica.SIunits.Time tsim = 40 "Simulation time";
          input Integer numberOfIntervalsin=10000 "No. of intervals";
          //input String methodin = "Rkfix4" "Solver";
          input String methodin = "DASSL" "Solver";
          input Real fixedstepsizein= 1e-4 "Time step - needed only for fixed time step solvers";
          //
          // DEFINING THE NONLINEAR EXPERIMENT, AND LINEAR EXPERIMENT MODELS
          //
          // 1) NONLINEAR EXPERIMENT: this is a model which applies a change to the input of the nonlinear model.
          // This model will be simulated, and the simulation results will be compared to the simulation of the corresponding linearized model.
          input String pathToNonlinearExperiment="SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAfterDisturbance.NonlinModel_for_NonlinExperiment" "Nonlinear experiment model";
          //
          // 2) LINEAR EXPERIMENT: this is a template that can be used for all three cases, so it is not necessary to create other cases here
          input String pathToLinearExperiment="SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAfterDisturbance.LinearModelExperiment";

        algorithm


          simulateModel(
            pathToNonlinearExperiment,
            stopTime=tsim,
            numberOfIntervals=numberOfIntervalsin,
            method = methodin,
            fixedstepsize=fixedstepsizein,
            resultFile="res_nl");
          simulateModel(
            pathToLinearExperiment,
            stopTime=tsim,
            numberOfIntervals=numberOfIntervalsin,
            method = methodin,
            fixedstepsize=fixedstepsizein,
            resultFile="res_lin");

        // plot

        createPlot(id=1, position={0, 2, 622, 900}, y={"Vt"}, range={30.0, 40.0, 0.985, 1.005}, autoscale=false, grid=true, filename="res_nl.mat", colors={{28,108,200}}, range2={0.55, 0.7000000000000001}, displayUnits={"1"});
        createPlot(id=1, position={0, 2, 622, 900}, y={"Q"}, range={30.0, 40.0, 0.35000000000000003, 0.45000000000000007}, autoscale=false, grid=true, subPlot=102, colors={{28,108,200}}, range2={0.30000000000000004, 0.5}, displayUnits={"1"});
        createPlot(id=1, position={0, 2, 622, 900}, y={"P"}, range={30.0, 40.0, 0.86, 0.94}, autoscale=false, grid=true, subPlot=103, colors={{28,108,200}}, range2={0.4, 0.8}, displayUnits={"1"});
        createPlot(id=1, position={0, 2, 622, 900}, y={"w"}, range={30.0, 40.0, 0.9995, 1.0005}, autoscale=false, grid=true, subPlot=104, colors={{28,108,200}}, range2={0.46, 0.54}, displayUnits={"1"});
        createPlot(id=1, position={0, 2, 622, 900}, y={"delta"}, range={30.0, 40.0, 1.55, 1.6500000000000001}, autoscale=false, grid=true, subPlot=105, colors={{28,108,200}}, range2={0.30000000000000004, 0.45000000000000007}, displayUnits={"rad"});
        createPlot(id=1, position={0, 2, 622, 900}, y={"Vt"}, range={30.0, 40.0, 0.985, 1.005}, erase=false, autoscale=false, grid=true, filename="res_lin.mat", colors={{238,46,47}}, range2={0.55, 0.7000000000000001});
        createPlot(id=1, position={0, 2, 622, 900}, y={"Q"}, range={30.0, 40.0, 0.35000000000000003, 0.45000000000000007}, erase=false, autoscale=false, grid=true, subPlot=102, colors={{238,46,47}}, range2={0.30000000000000004, 0.5});
        createPlot(id=1, position={0, 2, 622, 900}, y={"P"}, range={30.0, 40.0, 0.86, 0.94}, erase=false, autoscale=false, grid=true, subPlot=103, colors={{238,46,47}}, range2={0.4, 0.8});
        createPlot(id=1, position={0, 2, 622, 900}, y={"w"}, range={30.0, 40.0, 0.9995, 1.0005}, erase=false, autoscale=false, grid=true, subPlot=104, colors={{238,46,47}}, range2={0.46, 0.54});
        createPlot(id=1, position={0, 2, 622, 900}, y={"delta"}, range={30.0, 40.0, 1.55, 1.6500000000000001}, erase=false, autoscale=false, grid=true, subPlot=105, colors={{238,46,47}}, range2={0.30000000000000004, 0.45000000000000007});

        //removePlots(false);
        createPlot(id=2, position={308, 10, 734, 889}, y={"Vt"}, range={0.0, 40.0, 0.9, 1.05}, grid=true, filename="res_nl.mat", colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=2, position={308, 10, 734, 889}, y={"Q"}, range={0.0, 40.0, 0.0, 1.0}, grid=true, subPlot=102, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=2, position={308, 10, 734, 889}, y={"P"}, range={0.0, 40.0, 0.4, 1.2000000000000002}, grid=true, subPlot=103, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=2, position={308, 10, 734, 889}, y={"w"}, range={0.0, 40.0, 0.99, 1.01}, grid=true, subPlot=104, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=2, position={308, 10, 734, 889}, y={"delta"}, range={0.0, 40.0, 1.0, 2.5}, grid=true, subPlot=105, colors={{28,108,200}}, displayUnits={"rad"});
        createPlot(id=2, position={308, 10, 734, 889}, y={"Vt"}, range={0.0, 40.0, 0.9, 1.05}, erase=false, grid=true, filename="res_lin.mat", colors={{238,46,47}});
        createPlot(id=2, position={308, 10, 734, 889}, y={"Q"}, range={0.0, 40.0, 0.0, 1.0}, erase=false, grid=true, subPlot=102, colors={{238,46,47}});
        createPlot(id=2, position={308, 10, 734, 889}, y={"P"}, range={0.0, 40.0, 0.4, 1.2000000000000002}, erase=false, grid=true, subPlot=103, colors={{238,46,47}});
        createPlot(id=2, position={308, 10, 734, 889}, y={"w"}, range={0.0, 40.0, 0.99, 1.01}, erase=false, grid=true, subPlot=104, colors={{238,46,47}});
        createPlot(id=2, position={308, 10, 734, 889}, y={"delta"}, range={0.0, 40.0, 1.0, 2.5}, erase=false, grid=true, subPlot=105, colors={{238,46,47}});


          annotation(__Dymola_interactive=true, Documentation(info="<html>
<p>This function linearizes the model at two different times, initialization and at a user provided time.</p>
</html>"));
        end Compare;
      end LinAfterDisturbance;
    end LinearizeAfterDisturbance;

    package LinearizeAfterDisturbanceW4inputsAndManyOutputs
      extends Modelica.Icons.VariantsPackage;
      model NonlinModel_for_Simulation
        "Model that includes a line removal at 5 seconds for linearization at initialization and after the line removal"
        extends Modelica.Icons.Example;
        Modelica.Blocks.Interfaces.RealOutput Vt
          annotation (Placement(transformation(extent={{100,70},{120,90}}),
              iconTransformation(extent={{100,70},{120,90}})));
      public
        Modelica.Blocks.Interfaces.RealOutput Q
          annotation (Placement(transformation(extent={{100,-10},{120,10}}),
              iconTransformation(extent={{100,-10},{120,10}})));
        Modelica.Blocks.Interfaces.RealOutput P
          annotation (Placement(transformation(extent={{100,30},{120,50}}),
              iconTransformation(extent={{100,30},{120,50}})));
        Modelica.Blocks.Interfaces.RealOutput w
          annotation (Placement(transformation(extent={{100,-50},{120,-30}}),
              iconTransformation(extent={{100,-50},{120,-30}})));
        Modelica.Blocks.Interfaces.RealOutput delta
          annotation (Placement(transformation(extent={{100,-90},{120,-70}}),
              iconTransformation(extent={{100,-90},{120,-70}})));
        Modelica.Blocks.Sources.Constant uPSSchange(k=0)
          annotation (Placement(transformation(extent={{-142,30},{-122,50}})));
        Modelica.Blocks.Math.Gain PSSInputGain(k=1)  annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-92,40})));
        Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-92,0})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0) annotation (Placement(
              transformation(extent={{-140,-10},{-120,10}})));
        Modelica.Blocks.Sources.Constant Ploadchange(k=0) annotation (Placement(
              transformation(extent={{-142,-50},{-122,-30}})));
        Modelica.Blocks.Math.Gain uPloadInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-94,-40})));
        Interfaces.SMIB_AVR_PSS_wInput_wLineRmovaland4inputs
          sMIB_AVR_PSS_wInput_wLineRmovaland4inputs(
                                          t1=0.5)
          annotation (Placement(transformation(extent={{-50,-68},{70,52}})));
        Modelica.Blocks.Sources.Constant AVRvs_change(k=0)
                                                          annotation (Placement(
              transformation(extent={{-138,-90},{-118,-70}})));
        Modelica.Blocks.Math.Gain uPloadInputGain1(k=1)
                                                       annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-90,-80})));
      equation
        connect(PSSInputGain.u, uPSSchange.y)
          annotation (Line(points={{-104,40},{-121,40}}, color={0,0,127}));
        connect(Pmchange.y,pmInputGain. u)
          annotation (Line(points={{-119,0},{-104,0}},   color={0,0,127}));
        connect(Ploadchange.y,uPloadInputGain. u)
          annotation (Line(points={{-121,-40},{-106,-40}},
                                                         color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPSS, PSSInputGain.y)
          annotation (Line(points={{-56.3158,33.0526},{-65.1428,33.0526},{
                -65.1428,40},{-81,40}}, color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPm, pmInputGain.y)
          annotation (Line(points={{-56.3158,7.78947},{-64,7.78947},{-64,0},{
                -81,0}},
              color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPload,
          uPloadInputGain.y) annotation (Line(points={{-56.3158,-11.1579},{
                -65.2857,-11.1579},{-65.2857,-40},{-83,-40}}, color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Vt, Vt) annotation (
            Line(points={{73.1579,33.0526},{73.1579,55.9286},{110,55.9286},{110,
                80}}, color={0,0,127}));
        connect(P, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.P) annotation (
            Line(points={{110,40},{100,40},{100,20.4211},{73.1579,20.4211}},
              color={0,0,127}));
        connect(Q, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Q) annotation (
            Line(points={{110,0},{105,0},{105,7.78947},{73.1579,7.78947}},
                                                                 color={0,0,127}));
        connect(w, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.w) annotation (
            Line(points={{110,-40},{100,-40},{100,-4.84211},{73.1579,-4.84211}},
              color={0,0,127}));
        connect(delta, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.delta)
          annotation (Line(points={{110,-80},{92,-80},{92,-17.4737},{73.1579,
                -17.4737}}, color={0,0,127}));
        connect(AVRvs_change.y, uPloadInputGain1.u)
          annotation (Line(points={{-117,-80},{-102,-80}}, color={0,0,127}));
        connect(uPloadInputGain1.y, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uvsAVR)
          annotation (Line(points={{-79,-80},{-66,-80},{-66,-30.1053},{-56.3158,
                -30.1053}}, color={0,0,127}));
        annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                  -100},{100,100}})),                                  Diagram(
              coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                  100,100}})),
          experiment(
            StopTime=30,
            __Dymola_NumberOfIntervals=10000,
            __Dymola_Algorithm="Dassl"),
          __Dymola_Commands(file="MosScripts/busvoltages.mos" "busvoltages",
              file="MosScripts/outputs_lineswitch.mos" "outputs_lineswitch",
            file="MosScripts/generator_and_controls.mos"
              "generator_and_controls"));
      end NonlinModel_for_Simulation;

      package LinAtZero
        extends Modelica.Icons.ExamplesPackage;
        model NonlinModel_for_Linearization
          extends Modelica.Icons.Example;
          Modelica.Blocks.Interfaces.RealOutput Vt
            annotation (Placement(transformation(extent={{100,70},{120,90}}),
                iconTransformation(extent={{100,70},{120,90}})));
        public
          Modelica.Blocks.Interfaces.RealOutput Q
            annotation (Placement(transformation(extent={{100,-10},{120,10}}),
                iconTransformation(extent={{100,-10},{120,10}})));
          Modelica.Blocks.Interfaces.RealOutput P
            annotation (Placement(transformation(extent={{100,30},{120,50}}),
                iconTransformation(extent={{100,30},{120,50}})));
          Modelica.Blocks.Interfaces.RealOutput w
            annotation (Placement(transformation(extent={{100,-50},{120,-30}}),
                iconTransformation(extent={{100,-50},{120,-30}})));
          Modelica.Blocks.Interfaces.RealOutput delta
            annotation (Placement(transformation(extent={{100,-90},{120,-70}}),
                iconTransformation(extent={{100,-90},{120,-70}})));
          Modelica.Blocks.Interfaces.RealInput uPSS
            annotation (Placement(transformation(extent={{-140,40},{-100,80}})));
          Modelica.Blocks.Interfaces.RealInput uPm
            annotation (Placement(transformation(extent={{-140,-20},{-100,20}}),
                iconTransformation(extent={{-140,-20},{-100,20}})));
          Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
                transformation(extent={{-140,-60},{-100,-20}})));
          Interfaces.SMIB_AVR_PSS_wInput_wLineRmovaland4inputs
            sMIB_AVR_PSS_wInput_wLineRmovaland4inputs
            annotation (Placement(transformation(extent={{-40,-20},{40,60}})));
          Modelica.Blocks.Interfaces.RealInput uvsAVR annotation (Placement(
                transformation(extent={{-140,-100},{-100,-60}})));
          Modelica.Blocks.Routing.Multiplex4 Vs annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={-36,-70})));
          Modelica.Blocks.Interfaces.RealOutput BusVm[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={-36,-110})));
          Modelica.Blocks.Routing.Multiplex4 As annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={-12,-68})));
          Modelica.Blocks.Routing.Multiplex4 Ps annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={12,-68})));
          Modelica.Blocks.Routing.Multiplex4 Qs annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={36,-68})));
          Modelica.Blocks.Interfaces.RealOutput BusAn[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={-12,-110})));
          Modelica.Blocks.Interfaces.RealOutput Plines[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={12,-110})));
          Modelica.Blocks.Interfaces.RealOutput Qlines[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={38,-110})));
          Modelica.Blocks.Routing.Multiplex3 Adiffs annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={62,-70})));
          Modelica.Blocks.Interfaces.RealOutput BAnDiffs1toN[3]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=0,
                origin={70,-146})));
        equation
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPSS, uPSS)
            annotation (Line(points={{-44.2105,42.8571},{-69.5714,42.8571},{
                  -69.5714,60},{-120,60}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPm, uPm)
            annotation (Line(points={{-44.2105,20},{-84,20},{-84,0},{-120,0}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPload, uPload)
            annotation (Line(points={{-44.2105,2.85714},{-67.1428,2.85714},{
                  -67.1428,-40},{-120,-40}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Vt, Vt) annotation (
             Line(points={{42.1053,42.8571},{73.0714,42.8571},{73.0714,80},{110,
                  80}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.P, P) annotation (
              Line(points={{42.1053,31.4286},{72.0714,31.4286},{72.0714,40},{
                  110,40}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Q, Q) annotation (
              Line(points={{42.1053,20},{92,20},{92,0},{110,0}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.w, w) annotation (
              Line(points={{42.1053,8.57143},{88,8.57143},{88,-40},{110,-40}},
                color={0,0,127}));
          connect(uvsAVR, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uvsAVR)
            annotation (Line(points={{-120,-80},{-60,-80},{-60,-14.2857},{
                  -44.2105,-14.2857}}, color={0,0,127}));
          connect(Vs.y, BusVm) annotation (Line(points={{-36,-78.8},{-36,-110}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.delta, delta)
            annotation (Line(points={{42.1053,-2.85714},{84,-2.85714},{84,-80},
                  {110,-80}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm4, Vs.u4[1])
            annotation (Line(points={{-25.2632,-22.8571},{-25.2632,-51.4286},{
                  -43.2,-51.4286},{-43.2,-60.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm3, Vs.u3[1])
            annotation (Line(points={{-29.4737,-22.8571},{-29.4737,-51.4286},{
                  -38.4,-51.4286},{-38.4,-60.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm2, Vs.u2[1])
            annotation (Line(points={{-33.6842,-22.8571},{-33.6842,-51.4286},{
                  -33.6,-51.4286},{-33.6,-60.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm1, Vs.u1[1])
            annotation (Line(points={{-37.8947,-22.8571},{-37.8947,-51.4286},{
                  -28.8,-51.4286},{-28.8,-60.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva1, As.u1[1])
            annotation (Line(points={{-21.0526,-22.8571},{-21.0526,-50},{-4.8,
                  -50},{-4.8,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva2, As.u2[1])
            annotation (Line(points={{-16.8421,-22.8571},{-16.8421,-48},{-9.6,
                  -48},{-9.6,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva3, As.u3[1])
            annotation (Line(points={{-12.6316,-22.8571},{-12.6316,-52},{-14.4,
                  -52},{-14.4,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva4, As.u4[1])
            annotation (Line(points={{-8.42105,-22.8571},{-8.42105,-52},{-19.2,
                  -52},{-19.2,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline1, Ps.u1[1])
            annotation (Line(points={{-4.21053,-22.8571},{-4.21053,-50},{19.2,
                  -50},{19.2,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline2, Ps.u2[1])
            annotation (Line(points={{-1.77636e-15,-22.8571},{-1.77636e-15,-52},
                  {14.4,-52},{14.4,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline3, Ps.u3[1])
            annotation (Line(points={{4.21053,-22.8571},{4.21053,-54},{9.6,-54},
                  {9.6,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline4, Ps.u4[1])
            annotation (Line(points={{8.42105,-22.8571},{8.42105,-52},{4.8,-52},
                  {4.8,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline1, Qs.u1[1])
            annotation (Line(points={{12.6316,-22.8571},{12.6316,-50},{43.2,-50},
                  {43.2,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline2, Qs.u2[1])
            annotation (Line(points={{16.8421,-22.8571},{16.8421,-52},{38.4,-52},
                  {38.4,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline3, Qs.u3[1])
            annotation (Line(points={{21.0526,-22.8571},{21.0526,-48},{33.6,-48},
                  {33.6,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline4, Qs.u4[1])
            annotation (Line(points={{25.2632,-22.8571},{25.2632,-52},{28.8,-52},
                  {28.8,-58.4}}, color={0,0,127}));
          connect(As.y, BusAn) annotation (Line(points={{-12,-76.8},{-12,-110}},
                color={0,0,127}));
          connect(Ps.y, Plines)
            annotation (Line(points={{12,-76.8},{12,-110}}, color={0,0,127}));
          connect(Qlines, Qs.y) annotation (Line(points={{38,-110},{38,-76.8},{
                  36,-76.8}}, color={0,0,127}));
          connect(Adiffs.y, BAnDiffs1toN) annotation (Line(points={{62,-81},{62,
                  -146},{70,-146}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to2, Adiffs.u1
            [1]) annotation (Line(points={{29.4737,-22.8571},{29.4737,-34},{69,
                  -34},{69,-58}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to3, Adiffs.u2
            [1]) annotation (Line(points={{33.6842,-22.8571},{62,-22.8571},{62,
                  -58}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to4, Adiffs.u3
            [1]) annotation (Line(points={{37.8947,-22.8571},{37.8947,-52},{55,
                  -52},{55,-58}}, color={0,0,127}));
          annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
                coordinateSystem(preserveAspectRatio=false, extent={{-100,-160},
                    {100,100}})));
        end NonlinModel_for_Linearization;

        model NonlinModel_for_NonlinExperiment
          extends Modelica.Icons.Example;
          Modelica.Blocks.Interfaces.RealOutput Vt
            annotation (Placement(transformation(extent={{98,68},{118,90}})));
        public
          Modelica.Blocks.Interfaces.RealOutput Q
            annotation (Placement(transformation(extent={{98,-10},{118,12}})));
          Modelica.Blocks.Interfaces.RealOutput P
            annotation (Placement(transformation(extent={{98,30},{118,52}})));
          Modelica.Blocks.Interfaces.RealOutput w
            annotation (Placement(transformation(extent={{98,-50},{118,-28}})));
          Modelica.Blocks.Interfaces.RealOutput delta
            annotation (Placement(transformation(extent={{98,-92},{118,-70}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-100,20},{-80,40}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-100,-10},{-80,10}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.1,
            offset=0,
            startTime=1)                                    annotation (Placement(
                transformation(extent={{-100,-40},{-80,-20}})));
          Interfaces.SMIB_AVR_PSS_wInput_wLineRmovaland4inputs
            sMIB_AVR_PSS_wInput_wLineRmovaland4inputs
            annotation (Placement(transformation(extent={{-20,-18},{8,10}})));
          Modelica.Blocks.Sources.Constant AVRvs_change(k=0)
                                                            annotation (Placement(
                transformation(extent={{-100,-80},{-80,-60}})));
          Modelica.Blocks.Routing.Multiplex4 Vs annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={-50,-232})));
          Modelica.Blocks.Routing.Multiplex4 As annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={-26,-230})));
          Modelica.Blocks.Routing.Multiplex4 Ps annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={-2,-230})));
          Modelica.Blocks.Routing.Multiplex4 Qs annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={22,-230})));
          Modelica.Blocks.Routing.Multiplex3 Adiffs annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={48,-232})));
          Modelica.Blocks.Interfaces.RealOutput BusVm[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={-50,-272})));
          Modelica.Blocks.Interfaces.RealOutput BusAn[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={-26,-272})));
          Modelica.Blocks.Interfaces.RealOutput Plines[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={-2,-272})));
          Modelica.Blocks.Interfaces.RealOutput Qlines[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={24,-272})));
          Modelica.Blocks.Interfaces.RealOutput BAnDiffs1toN[3]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=0,
                origin={70,-276})));
        equation
          connect(PSSchange.y, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPSS)
            annotation (Line(points={{-79,30},{-52,30},{-52,4},{-21.4737,4}},
                color={0,0,127}));
          connect(Pmchange.y, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPm)
            annotation (Line(points={{-79,0},{-50,0},{-50,-4},{-21.4737,-4}},
                color={0,0,127}));
          connect(Ploadchange.y, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPload)
            annotation (Line(points={{-79,-30},{-50,-30},{-50,-10},{-21.4737,
                  -10}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Vt, Vt) annotation (
             Line(points={{8.73684,4},{55.5,4},{55.5,79},{108,79}}, color={0,0,
                  127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.P, P) annotation (
              Line(points={{8.73684,0},{55.5,0},{55.5,41},{108,41}}, color={0,0,
                  127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Q, Q) annotation (
              Line(points={{8.73684,-4},{55.5,-4},{55.5,1},{108,1}}, color={0,0,
                  127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.w, w) annotation (
              Line(points={{8.73684,-8},{54.5,-8},{54.5,-39},{108,-39}}, color=
                  {0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.delta, delta)
            annotation (Line(points={{8.73684,-12},{56.5,-12},{56.5,-81},{108,
                  -81}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uvsAVR,
            AVRvs_change.y) annotation (Line(points={{-21.4737,-16},{-40,-16},{
                  -40,-70},{-79,-70}}, color={0,0,127}));
          connect(Vs.y, BusVm) annotation (Line(points={{-50,-240.8},{-50,-272}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm4, Vs.u4[1])
            annotation (Line(points={{-14.8421,-19},{-14.8421,-187.429},{-57.2,
                  -187.429},{-57.2,-222.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm3, Vs.u3[1])
            annotation (Line(points={{-16.3158,-19},{-16.3158,-187.429},{-52.4,
                  -187.429},{-52.4,-222.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm2, Vs.u2[1])
            annotation (Line(points={{-17.7895,-19},{-17.7895,-187.429},{-47.6,
                  -187.429},{-47.6,-222.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm1, Vs.u1[1])
            annotation (Line(points={{-19.2632,-19},{-19.2632,-187.429},{-42.8,
                  -187.429},{-42.8,-222.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva1, As.u1[1])
            annotation (Line(points={{-13.3684,-19},{-13.3684,-186},{-18.8,-186},
                  {-18.8,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva2, As.u2[1])
            annotation (Line(points={{-11.8947,-19},{-11.8947,-184},{-23.6,-184},
                  {-23.6,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva3, As.u3[1])
            annotation (Line(points={{-10.4211,-19},{-10.4211,-188},{-28.4,-188},
                  {-28.4,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva4, As.u4[1])
            annotation (Line(points={{-8.94737,-19},{-8.94737,-188},{-33.2,-188},
                  {-33.2,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline1, Ps.u1[1])
            annotation (Line(points={{-7.47368,-19},{-7.47368,-186},{5.2,-186},
                  {5.2,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline2, Ps.u2[1])
            annotation (Line(points={{-6,-19},{-6,-188},{0.4,-188},{0.4,-220.4}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline3, Ps.u3[1])
            annotation (Line(points={{-4.52632,-19},{-4.52632,-190},{-4.4,-190},
                  {-4.4,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline4, Ps.u4[1])
            annotation (Line(points={{-3.05263,-19},{-3.05263,-188},{-9.2,-188},
                  {-9.2,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline1, Qs.u1[1])
            annotation (Line(points={{-1.57895,-19},{-1.57895,-186},{29.2,-186},
                  {29.2,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline2, Qs.u2[1])
            annotation (Line(points={{-0.105263,-19},{-0.105263,-188},{24.4,
                  -188},{24.4,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline3, Qs.u3[1])
            annotation (Line(points={{1.36842,-19},{1.36842,-184},{19.6,-184},{
                  19.6,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline4, Qs.u4[1])
            annotation (Line(points={{2.84211,-19},{2.84211,-188},{14.8,-188},{
                  14.8,-220.4}}, color={0,0,127}));
          connect(As.y, BusAn) annotation (Line(points={{-26,-238.8},{-26,-272}},
                color={0,0,127}));
          connect(Ps.y, Plines)
            annotation (Line(points={{-2,-238.8},{-2,-272}}, color={0,0,127}));
          connect(Qlines, Qs.y) annotation (Line(points={{24,-272},{24,-238.8},
                  {22,-238.8}}, color={0,0,127}));
          connect(Adiffs.y, BAnDiffs1toN) annotation (Line(points={{48,-243},{
                  48,-276},{70,-276}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to2, Adiffs.u1
            [1]) annotation (Line(points={{4.31579,-19},{4.31579,-170},{55,-170},
                  {55,-220}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to3, Adiffs.u2
            [1]) annotation (Line(points={{5.78947,-19},{2,-19},{2,-220},{48,
                  -220}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to4, Adiffs.u3
            [1]) annotation (Line(points={{7.26316,-19},{7.26316,-188},{41,-188},
                  {41,-220}}, color={0,0,127}));
          annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
                coordinateSystem(preserveAspectRatio=false, extent={{-100,-300},
                    {100,100}})),
            experiment(StopTime=40, __Dymola_Algorithm="Dassl"));
        end NonlinModel_for_NonlinExperiment;

        model LinearModelExperiment "Simulate the linearized model"
          extends Modelica.Icons.Example;
          extends
            SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
          inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
            annotation (Placement(transformation(extent={{72,70},{92,90}})));
          Modelica.Blocks.Routing.Multiplex4 multiplex4_1(n1=1, n2=1)
            annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
          Modelica_LinearSystems2.Controller.StateSpace stateSpace(system(
              A=[0.0,376.9911183891705,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0; -0.1746304332368294,
                  0.0,0.0,0.0,-0.19691541972503057,0.05165083246537863,0.0,0.0,0.0,0.0,0.0,
                  0.0; -0.2572522810106802,0.0,-0.12500000002769562,0.0,-0.26676263837906566,
                  -0.0011575290369758742,0.0,0.0,0.1250000000049628,0.0,0.0,0.0; 0.4001901909678766,
                  0.0,0.0,-0.999999999888219,0.007949510715029454,-1.490106156462758,0.0,
                  0.0,0.0,0.0,0.0,0.0; -3.387125382189737,0.0,33.333333332497816,0.0,-36.84567732550863,
                  -0.01524066579267384,0.0,0.0,0.0,0.0,0.0,0.0; 2.27536016432831,0.0,0.0,
                  14.285714284117413,0.0451985088472567,-22.758006374191766,0.0,0.0,0.0,
                  0.0,0.0,0.0; -9.193382910515082,0.0,0.0,0.0,29.09591589061873,33.29601284357062,
                  -66.66666667044463,0.0,0.0,0.0,0.0,0.0; 0.0,0.0,0.0,0.0,0.0,0.0,0.0,-1.0,
                  0.0,0.0,0.0,0.0; 0.0,18999999.99623949,0.0,0.0,0.0,0.0,-2000000.0001133387,
                  10000.00082740371,-10000.000000397024,0.0,0.0,-18999999.99975344; 0.0,
                  0.009499999998119747,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.001,0.0,-0.009499999999876722;
                  0.0,0.009499999998119747,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.001,-0.009499999999876722;
                  0.0,0.7092198580777589,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.7092198582337657],
              B=[0.0,0.0,0.0,0.0; 0.0,0.14285715467719584,-0.03153854954973667,0.0; 0.0,
                  0.0,-0.04074007797782997,0.0; 0.0,0.0,0.09771095044186495,0.0; 0.0,0.0,
                  -0.5364061047193521,0.0; 0.0,0.0,0.555554411997759,0.0; 0.0,0.0,-1.05756144582377,
                  0.0; 0.0,0.0,0.0,0.0; 18999998.019353367,0.0,0.0,2000000.1654807418; 0.009499999009676685,
                  0.0,0.0,0.0; 0.009499999009676685,0.0,0.0,0.0; 0.7092199168371426,0.0,
                  0.0,0.0],
              C=[-0.1379007436577262,0.0,0.0,0.0,0.43643873835928093,0.49944019265355927,
                  0.0,0.0,0.0,0.0,0.0,0.0; 0.31220916813813043,0.0,0.0,0.0,1.4657744246489735,
                  0.7706077401153867,0.0,0.0,0.0,0.0,0.0,0.0; 1.2147745075522633,0.0,0.0,
                  0.0,1.371466144301296,-0.3580014284642535,0.0,0.0,0.0,0.0,0.0,0.0; 0.0,
                  0.9999999998896401,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0; 0.999999999878488,
                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0; -0.1379007436577262,0.0,0.0,
                  0.0,0.43643873835928093,0.49944019265355927,0.0,0.0,0.0,0.0,0.0,0.0; -0.15938762699646958,
                  0.0,0.0,0.0,0.2477333003245865,0.37835342914912573,0.0,0.0,0.0,0.0,0.0,
                  0.0; 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0; -0.11352059682667301,
                  0.0,0.0,0.0,0.09329433533779881,0.20504814418556377,0.0,0.0,0.0,0.0,0.0,
                  0.0; 40.265026691359,0.0,0.0,0.0,29.426548560089255,-24.288631960317172,
                  0.0,0.0,0.0,0.0,0.0,0.0; 27.10367807265963,0.0,0.0,0.0,22.772208260589643,
                  -14.052649470892437,0.0,0.0,0.0,0.0,0.0,0.0; 0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0; 12.654328579482,0.0,0.0,0.0,12.395094422047636,-5.194900715168893,
                  0.0,0.0,0.0,0.0,0.0,0.0; 270.5593888756389,0.0,0.0,0.0,303.8201066846064,
                  -81.00487805949521,0.0,0.0,0.0,0.0,0.0,0.0; 938.1633800735949,0.0,0.0,
                  0.0,1069.8241492098143,-268.2314635855691,0.0,0.0,0.0,0.0,0.0,0.0; 946.6444961005402,
                  0.0,0.0,0.0,1062.8541358706382,-283.55059612402806,0.0,0.0,0.0,0.0,0.0,
                  0.0; 1488.0766383678153,0.0,0.0,0.0,1671.0105843624447,-445.5268305641155,
                  0.0,0.0,0.0,0.0,0.0,0.0; -16.302268461141338,0.0,0.0,0.0,247.6801730499653,
                  210.97619396012865,0.0,0.0,0.0,0.0,0.0,0.0; -48.80719047725832,0.0,0.0,
                  0.0,873.5589904833647,733.9421952393635,0.0,0.0,0.0,0.0,0.0,0.0; -380.75928569303505,
                  0.0,0.0,0.0,571.1094959510318,887.8068717152541,0.0,0.0,0.0,0.0,0.0,0.0;
                  -89.66247678907226,0.0,0.0,0.0,1362.2409512174486,1160.3690662124427,0.0,
                  0.0,0.0,0.0,0.0,0.0; 13.161348618699366,0.0,0.0,0.0,6.654340299499612,
                  -10.235982489424735,0.0,0.0,0.0,0.0,0.0,0.0; 40.265026691359,0.0,0.0,0.0,
                  29.426548560089255,-24.288631960317172,0.0,0.0,0.0,0.0,0.0,0.0; 27.610698109827307,
                  0.0,0.0,0.0,17.03145414015684,-19.09373124379204,0.0,0.0,0.0,0.0,0.0,0.0],
              D=[0.0,0.0,-0.01586342168735655,0.0; 0.0,0.0,0.021775442560212355,0.0; 0.0,
                  0.0,0.2194778803144004,0.0; 0.0,0.0,0.0,0.0; 0.0,0.0,0.0,0.0; 0.0,0.0,
                  -0.01586342168735655,0.0; 0.0,0.0,-0.014585777030617917,0.0; 0.0,0.0,0.0,
                  0.0; 0.0,0.0,-0.0024640289808530724,0.0; 0.0,0.0,-2.997332160248334,0.0;
                  0.0,0.0,-5.187335005985005,0.0; 0.0,0.0,0.0,0.0; 0.0,0.0,-16.538669100896186,
                  0.0; 0.0,0.0,-60.82046866140444,0.0; 0.0,0.0,882.5721806715592,0.0; 0.0,
                  0.0,-1337.2439298109384,0.0; 0.0,0.0,-334.51078707003035,0.0; 0.0,0.0,
                  -27.76100949120064,0.0; 0.0,0.0,85.34333062470978,0.0; 0.0,0.0,-198.70521583698064,
                  0.0; 0.0,0.0,-152.68498998466384,0.0; 0.0,0.0,2.1900028457366716,0.0;
                  0.0,0.0,-2.997332160248334,0.0; 0.0,0.0,13.541338717004692,0.0],
              yNames={"Vt","Q","P","w","delta","BusVm[1]","BusVm[2]","BusVm[3]","BusVm[4]","BusAn[1]",
                  "BusAn[2]","BusAn[3]","BusAn[4]","Plines[1]","Plines[2]","Plines[3]","Plines[4]",
                  "Qlines[1]","Qlines[2]","Qlines[3]","Qlines[4]","BAnDiffs1toN[1]","BAnDiffs1toN[2]",
                  "BAnDiffs1toN[3]"},
              xNames={"sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.delta","sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.w",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.e1q","sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.e1d",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.e2q","sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.e2d",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.avr.vm","sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.avr.vr",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.avr.vf1","sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.pss.imLeadLag.TF.x_scaled[1]",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.pss.imLeadLag1.TF.x_scaled[1]","sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.pss.derivativeLag.TF.x_scaled[1]"},
              uNames={"uPSS","uPm","uPload","uvsAVR"}),
                                      blockType=Modelica_LinearSystems2.Controller.Types.BlockTypeWithGlobalDefault.Continuous)
            annotation (Placement(transformation(extent={{-36,-10},{-16,10}})));

          Modelica.Blocks.Math.Add addy[24]
            annotation (Placement(transformation(extent={{6,-16},{26,4}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-124,-10},{-104,10}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.1,
            offset=0,
            startTime=1)                                    annotation (Placement(
                transformation(extent={{-124,-42},{-104,-22}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-122,20},{-102,40}})));
          Modelica.Blocks.Sources.Constant y0_initial[24](k={0.999999582767487,0.200358614325523,
                0.899999439716339,1,1.29897058010101,0.999999582767487,0.979295551776886,
                1,0.978660404682159,24.9878368377686,17.0641422271729,0,7.98409032821655,
                196.29345703125,722.09130859375,648.975646972656,1079.61401367188,15.5986824035645,
                60.3069686889648,-54.4150314331055,85.7927551269531,7.92369508743286,24.9878368377686,
                17.0037479400635}) annotation (Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={0,-52})));
          Modelica.Blocks.Sources.Constant AVRvs_change(k=0)
                                                            annotation (Placement(
                transformation(extent={{-122,-80},{-102,-60}})));
          Modelica.Blocks.Routing.DeMultiplex y(n=24)
            annotation (Placement(transformation(extent={{40,-16},{60,4}})));
          Modelica.Blocks.Interfaces.RealOutput BusVm[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={30,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput BusAn[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={50,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput Plines[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={70,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput Qlines[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={92,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput BAnDiffs1toN[3] annotation (
              Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={110,-50}), iconTransformation(extent={{140,30},{160,50}})));
        equation
          connect(multiplex4_1.y, stateSpace.u)
            annotation (Line(points={{-59,0},{-38,0}}, color={0,0,127}));
          connect(stateSpace.y, addy.u1)
            annotation (Line(points={{-15,0},{4,0}}, color={0,0,127}));
          connect(Pmchange.y,multiplex4_1. u2[1]) annotation (Line(points={{-103,0},{-92,
                  0},{-92,3},{-82,3}},     color={0,0,127}));
          connect(Ploadchange.y,multiplex4_1. u3[1]) annotation (Line(points={{-103,-32},
                  {-96,-32},{-96,-3},{-82,-3}},           color={0,0,127}));
          connect(PSSchange.y,multiplex4_1. u1[1]) annotation (Line(points={{-101,30},{-92,
                  30},{-92,9},{-82,9}},               color={0,0,127}));
          connect(y0_initial.y, addy.u2)
            annotation (Line(points={{0,-41},{0,-12},{4,-12}}, color={0,0,127}));
          connect(AVRvs_change.y, multiplex4_1.u4[1]) annotation (Line(points={{-101,-70},
                  {-92,-70},{-92,-9},{-82,-9}}, color={0,0,127}));
          connect(addy.y, y.u)
            annotation (Line(points={{27,-6},{38,-6}}, color={0,0,127}));
          connect(Vt, y.y[1]) annotation (Line(points={{150,80},{110,80},{110,
                  0.708333},{60,0.708333}}, color={0,0,127}));
          connect(Q, y.y[2]) annotation (Line(points={{150,0},{112,0},{112,
                  0.125},{60,0.125}}, color={0,0,127}));
          connect(P, y.y[3]) annotation (Line(points={{150,40},{112,40},{112,
                  -0.458333},{60,-0.458333}}, color={0,0,127}));
          connect(w, y.y[4]) annotation (Line(points={{150,-40},{126,-40},{126,
                  -1.04167},{60,-1.04167}}, color={0,0,127}));
          connect(delta, y.y[5]) annotation (Line(points={{150,-80},{131,-80},{
                  131,-1.625},{60,-1.625}}, color={0,0,127}));
          connect(y.y[6:9], BusVm) annotation (Line(points={{60,-3.95833},{60,-26},
                  {30,-26},{30,-50}}, color={0,0,127}));
          connect(BusAn, y.y[10:13]) annotation (Line(points={{50,-50},{50,-28},
                  {64,-28},{64,-6.29167},{60,-6.29167}}, color={0,0,127}));
          connect(y.y[14:17], Plines) annotation (Line(points={{60,-8.625},{70,
                  -8.625},{70,-50}}, color={0,0,127}));
          connect(y.y[18:21], Qlines) annotation (Line(points={{60,-10.9583},{
                  76,-10.9583},{76,-8},{92,-8},{92,-50}}, color={0,0,127}));
          connect(BAnDiffs1toN, y.y[22:24]) annotation (Line(points={{110,-50},
                  {110,-12.7083},{60,-12.7083}}, color={0,0,127}));
          annotation (
            Icon(coordinateSystem(preserveAspectRatio=false)),
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{140,100}}),                           graphics={
                Text(
                  extent={{-58,24},{-44,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="du",
                  fontSize=24),
                Text(
                  extent={{-38,-18},{-22,-38}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="y0",
                  fontSize=24),
                Text(
                  extent={{40,20},{50,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="y"),
                Text(
                  extent={{-12,22},{2,6}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="dy"),
                Text(
                  extent={{-80,80},{0,60}},
                  lineColor={85,170,255},
                  fillPattern=FillPattern.HorizontalCylinder,
                  fillColor={28,108,200},
                  horizontalAlignment=TextAlignment.Left,
                  textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
                Line(
                  points={{-4,64},{24,44},{18,14}},
                  color={0,0,255},
                  thickness=0.5,
                  arrow={Arrow.None,Arrow.Filled},
                  smooth=Smooth.Bezier)}),
            experiment(
              StopTime=20,
              __Dymola_NumberOfIntervals=10000,
              Tolerance=1e-06,
              __Dymola_fixedstepsize=0.01,
              __Dymola_Algorithm="Dassl"),
            __Dymola_Commands(file="MosScripts/linearize_and_compare.mos"
                "linearize_and_compare", file=
                  "MosScripts/linearize_and_compare.mos"
                "linearize_and_compare"));
        end LinearModelExperiment;

        function LinearizeSimple
          // Import things needed for the calculations
          import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
          // Declare outputs to display
          output Real A[:,:] "A-matrix";
          output Real B[:,:] "B-matrix";
          output Real C[:,:] "C-matrix";
          output Real D[:,:] "D-matrix";
          output String inputNames[:] "Modelica names of inputs";
          output String outputNames[:] "Modelica names of outputs";
          output String stateNames[:] "Modelica names of states";
          // Declare reconfigurable inputs
          input Modelica.SIunits.Time tlin = 20 "t for model linearization";
          input Modelica.SIunits.Time tsim = 20 "Simulation time";
          input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbanceW4inputsAndManyOutputs.LinAtZero.NonlinModel_for_Linearization";
          input String pathToNonlinearExperiment = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbanceW4inputsAndManyOutputs.LinAtZero.NonlinModel_for_NonlinExperiment";
        algorithm
          // Compute and display the ABCD matrices, etc
          Modelica.Utilities.Streams.print("Linearized Model");
          (A,B,C,D,inputNames,outputNames,stateNames) :=
            Modelica_LinearSystems2.Utilities.Import.linearize(
            pathToNonlinearPlantModel,tlin);
          nx := size(A, 1); //number of states
          Modelica.Utilities.Streams.print("Number of states" + String(nx));
          // Now we want to extract the initial value of the outputs to use it in our
          // linear model response
          Modelica.Utilities.Streams.print("Simulating nonlinear model");
          simulateModel(
            pathToNonlinearExperiment,
            stopTime=tsim,
            numberOfIntervals=1000,
            resultFile="res_nl");
           ny := size(C, 1);
           y0 := DymolaCommands.Trajectories.readTrajectory(
             "res_nl.mat",
             {outputNames[i] for i in 1:ny},
             DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat"));
           DataFiles.writeMATmatrix(
             "MyData.mat",
             "y0",
             [y0[1:ny,1]],
             append=true);
          // Print y0's first values which is needed for the linear response model
          y0out := y0[:,1]; // we only want the first few elements
          Modelica.Utilities.Streams.print("y0 =");
          Modelica.Math.Vectors.toString(y0out);
          annotation(__Dymola_interactive=true);
        end LinearizeSimple;

        function LinearizeAndCompare
          "Linearizes the model at initialization and after a disturbance at a given time"
          // See the Documentation for an explanation of the goals.
          // IMPORTING FUNCTIONS
          // Import things needed for the calculations
          import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
          // OUTPUTS OF THEFUNCTION - FOR DISPLAY
          // Declare outputs to display
          output Real A[:,:] "A-matrix";
          output Real B[:,:] "B-matrix";
          output Real C[:,:] "C-matrix";
          output Real D[:,:] "D-matrix";
          output String inputNames[:] "Modelica names of inputs";
          output String outputNames[:] "Modelica names of outputs";
          output String stateNames[:] "Modelica names of states";
          output Real y0out[:] "Initial value of the output variables";
          // INPUTS TO THE FUNCTION
          // Declare reconfigurable simulation parameters
          input Modelica.SIunits.Time tlin = 0 "t for model linearization";
          input Modelica.SIunits.Time tsim = 40 "Simulation time";
          input Real numberOfIntervalsin=10000 "No. of intervals";
          // Use this for Case A
          //input String method = "Rkfix4" "Solver";
          input String methodin = "DASSL" "Solver";
          input Real fixedstepsizein= 1e-12 "Time step - needed only for fixed time step solvers";
          //
          // DEFINING THE NONLINEAR PLANT, NONLINEAR EXPERIMENT, AND LINEAR EXPERIMENT MODELS
          //
          // 1) NONLINEAR PLANT:
          // This is the model that will be linearized, i.e. the nonlinear plant model
           input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbanceW4inputsAndManyOutputs.LinAtZero.NonlinModel_for_Linearization" "Nonlinear plant model";
          //
          //
          // 2) NONLINEAR EXPERIMENT: this is a model which applies a change to the input of the nonlinear model.
          // It must match the nonlinar plant above.
          // This model will be simulated, and the simulation results will be compared to the simulation of the corresponding linearized model.
          input String pathToNonlinearExperiment= "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbanceW4inputsAndManyOutputs.LinAtZero.NonlinModel_for_NonlinExperiment" "Nonlinear experiment model";
          //
          //
          // 3) LINEAR EXPERIMENT: this is a template that can be used for all three cases, so it is not necessary to create other cases here
          input String pathToLinearExperiment = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbanceW4inputsAndManyOutputs.LinAtZero.LinearModelGeneral";

        algorithm
          // Compute and display the ABCD matrices, etc
          (A,B,C,D,inputNames,outputNames,stateNames) :=
            Modelica_LinearSystems2.Utilities.Import.linearize(
            pathToNonlinearPlantModel,tlin);
          // LINEARIZE plant model at t_lin
          // This is the same as above, however, it stores it in a StateSpace object
          ss := Modelica_LinearSystems2.ModelAnalysis.Linearize(
            pathToNonlinearPlantModel, simulationSetup=
            Modelica_LinearSystems2.Records.SimulationOptionsForLinearization(
            linearizeAtInitial=false, t_linearize=tlin));
          // PRINT linear system
          Modelica.Utilities.Streams.print(String(ss));
          // SAVE the data in a mat file
          DataFiles.writeMATmatrix(
            "MyData.mat",
            "ABCD",
            [ss.A, ss.B; ss.C, ss.D],
            append=false);
          nx := size(ss.A, 1);
          DataFiles.writeMATmatrix(
            "MyData.mat",
            "nx",
            [nx],
            append=true);
          Modelica.Utilities.Streams.print("Simulating nonlinear model");
          simulateModel(
            pathToNonlinearExperiment,
            stopTime=tsim,
            numberOfIntervals=numberOfIntervalsin, method = methodin, fixedstepsize=fixedstepsizein,
            resultFile="res_nl");
           ny := size(ss.C, 1);
           y0 := DymolaCommands.Trajectories.readTrajectory(
             "res_nl.mat",
             {ss.yNames[i] for i in 1:ny},
             DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat"));
            // {"PS_ConstantEfd." + ss.yNames[i] for i in 1:ny},
           DataFiles.writeMATmatrix(
             "MyData.mat",
             "y0",
             [y0[1:ny,1]],
             append=true);
          // Print y0's first values which is needed for the linear response model
          y0out := y0[:,1]; // we only want the first few elements
          Modelica.Utilities.Streams.print("y0 =");
          Modelica.Math.Vectors.toString(y0out);
          //
          // We now simulate the linear model, which requires y0
          Modelica.Utilities.Streams.print("Simulating linear model");
          simulateModel(
            pathToLinearExperiment,
            stopTime=tsim,
            numberOfIntervals=numberOfIntervalsin, method = methodin, fixedstepsize=fixedstepsizein,
            resultFile="res_lin");
            // Plot
        removePlots(true);
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Vt"}, range={0.0, 20.0, 0.998, 1.002}, grid=true, filename="res_nl.mat", colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Q"}, range={0.0, 20.0, 0.18, 0.21}, grid=true, subPlot=102, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"P"}, range={0.0, 20.0, 0.86, 0.94}, grid=true, subPlot=103, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"w"}, range={0.0, 20.0, 0.9996, 1.0004}, grid=true, subPlot=104, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"delta"}, range={0.0, 20.0, 1.27, 1.3}, grid=true, subPlot=105, colors={{28,108,200}}, displayUnits={"rad"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Vt"}, range={0.0, 20.0, 0.998, 1.002}, erase=false, grid=true, filename="res_lin.mat", colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Q"}, range={0.0, 20.0, 0.18, 0.21}, erase=false, grid=true, subPlot=102, colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"P"}, range={0.0, 20.0, 0.86, 0.94}, erase=false, grid=true, subPlot=103, colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"w"}, range={0.0, 20.0, 0.9996, 1.0004}, erase=false, grid=true, subPlot=104, colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"delta"}, range={0.0, 20.0, 1.27, 1.3}, erase=false, grid=true, subPlot=105, colors={{238,46,47}});

          annotation(__Dymola_interactive=true, Documentation(info="<html>
<p>This function linearizes the model at two different times, initialization and at a user provided time.</p>
</html>"));
        end LinearizeAndCompare;

        model LinearModelGeneral
          "Simulate the linearized SMIB model obtained by running the function LinearizeSMIB."
          extends SMIBPS_IdControl.Utilities.Icons.FunctionDependentExample;
          extends
            SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
          // The following definitions are very important to couple the linear model
          // to the linearization of the nonlinear model and the simulation
          parameter Real[:] y0=vector(DataFiles.readMATmatrix("MyData.mat", "y0")) annotation (Evaluate=false);
          // The following has to be imported in order to be able to interpret and manipulate the StateSpace types
          import Modelica_LinearSystems2.StateSpace;
          parameter StateSpace ss=StateSpace.Import.fromFile("MyData.mat", "ABCD");
          parameter Integer ny=size(ss.C, 1);
          inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
            annotation (Placement(transformation(extent={{60,60},{80,80}})));
          Modelica.Blocks.Routing.Multiplex4 multiplex4_1(n1=1, n2=1)
            annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
          Modelica.Blocks.Math.Add addy[ny]
            annotation (Placement(transformation(extent={{6,-16},{26,4}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-122,-10},{-102,10}})));
          Modelica.Blocks.Sources.Constant y0_initial[ny](k=y0)      annotation (
              Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={0,-32})));
          Modelica_LinearSystems2.Controller.StateSpace stateSpace(system=ss)
            annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-120,20},{-100,40}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.1,
            offset=0,
            startTime=1)                                    annotation (Placement(
                transformation(extent={{-120,-40},{-100,-20}})));
          Modelica.Blocks.Sources.Constant AVRvs_change(k=0)
                                                            annotation (Placement(
                transformation(extent={{-120,-80},{-100,-60}})));
          Modelica.Blocks.Routing.DeMultiplex y(n=24)
            annotation (Placement(transformation(extent={{38,-16},{58,4}})));
          Modelica.Blocks.Interfaces.RealOutput BusVm[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={30,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput BusAn[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={50,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput Plines[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={70,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput Qlines[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={92,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput BAnDiffs1toN[3] annotation (
              Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={110,-50}), iconTransformation(extent={{140,30},{160,50}})));
        equation
          connect(Pmchange.y,multiplex4_1. u2[1]) annotation (Line(points={{-101,0},
                  {-92,0},{-92,3},{-82,3}},color={0,0,127}));
          connect(y0_initial.y, addy.u2)
            annotation (Line(points={{0,-21},{0,-12},{4,-12}}, color={0,0,127}));
          connect(multiplex4_1.y, stateSpace.u)
            annotation (Line(points={{-59,0},{-42,0}}, color={0,0,127}));
          connect(stateSpace.y, addy.u1)
            annotation (Line(points={{-19,0},{4,0}}, color={0,0,127}));
          connect(PSSchange.y,multiplex4_1. u1[1]) annotation (Line(points={{-99,30},
                  {-92,30},{-92,9},{-82,9}},         color={0,0,127}));
          connect(Ploadchange.y,multiplex4_1. u3[1]) annotation (Line(points={{-99,-30},
                  {-90,-30},{-90,-3},{-82,-3}},          color={0,0,127}));
          connect(AVRvs_change.y, multiplex4_1.u4[1]) annotation (Line(points={
                  {-99,-70},{-86,-70},{-86,-9},{-82,-9}}, color={0,0,127}));
          connect(addy.y,y. u)
            annotation (Line(points={{27,-6},{36,-6}}, color={0,0,127}));
          connect(Vt, y.y[1]) annotation (Line(points={{150,80},{100,80},{100,
                  0.708333},{58,0.708333}}, color={0,0,127}));
          connect(Q, y.y[2]) annotation (Line(points={{150,0},{106,0},{106,
                  0.125},{58,0.125}}, color={0,0,127}));
          connect(P, y.y[3]) annotation (Line(points={{150,40},{102,40},{102,
                  -0.458333},{58,-0.458333}}, color={0,0,127}));
          connect(w, y.y[4]) annotation (Line(points={{150,-40},{126,-40},{126,
                  -12},{58,-12},{58,-1.04167}}, color={0,0,127}));
          connect(delta, y.y[5]) annotation (Line(points={{150,-80},{130,-80},{
                  130,-1.625},{58,-1.625}}, color={0,0,127}));
          connect(BusVm, y.y[6:9]) annotation (Line(points={{30,-50},{30,-20},{
                  58,-20},{58,-3.95833}}, color={0,0,127}));
          connect(BusAn, y.y[10:13]) annotation (Line(points={{50,-50},{50,-22},
                  {62,-22},{62,-6.29167},{58,-6.29167}}, color={0,0,127}));
          connect(Plines, y.y[14:17]) annotation (Line(points={{70,-50},{70,-8},
                  {58,-8},{58,-8.625}}, color={0,0,127}));
          connect(Qlines, y.y[18:21]) annotation (Line(points={{92,-50},{92,
                  -10.9583},{58,-10.9583}}, color={0,0,127}));
          connect(BAnDiffs1toN, y.y[22:24]) annotation (Line(points={{110,-50},
                  {110,-8},{58,-8},{58,-12.7083}}, color={0,0,127}));
          annotation (
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {140,100}})),
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{140,100}}),                           graphics={
                Text(
                  extent={{-58,24},{-44,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="du",
                  fontSize=24),
                Text(
                  extent={{-12,20},{-2,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="dy",
                  fontSize=24),
                Text(
                  extent={{-38,-18},{-22,-38}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="y0",
                  fontSize=24),
                Text(
                  extent={{40,20},{50,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="y"),
                Text(
                  extent={{-80,80},{0,60}},
                  lineColor={85,170,255},
                  fillPattern=FillPattern.HorizontalCylinder,
                  fillColor={28,108,200},
                  horizontalAlignment=TextAlignment.Left,
                  textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
                Text(
                  extent={{-80,-80},{80,-100}},
                  lineColor={85,170,255},
                  fillPattern=FillPattern.HorizontalCylinder,
                  fillColor={28,108,200},
                  horizontalAlignment=TextAlignment.Left,
                  textString="Notice the change in the order of the outputs w.r.t. the nonlinear model.
They have to be rearranged based on the order provided by the linearization function."),
                Line(
                  points={{66,-86},{128,-82},{140,-8}},
                  color={0,0,255},
                  thickness=0.5,
                  arrow={Arrow.None,Arrow.Filled},
                  smooth=Smooth.Bezier),
                Line(
                  points={{-4,68},{24,48},{18,18}},
                  color={0,0,255},
                  thickness=0.5,
                  arrow={Arrow.None,Arrow.Filled},
                  smooth=Smooth.Bezier)}),
            experiment(
              StopTime=15,
              __Dymola_NumberOfIntervals=1000,
              __Dymola_Algorithm="Dassl"),
            Documentation(info="<html>
<p>DO NOT try to run this model on it&apos;s own! </p>
<p>Models with this icon will not simulate on their own, instead they work together with a function that populates certain parameters in the model and perform other operations.</p>
<p><br>See the associated function to run:<span style=\"color: #1c6cc8;\"> <span style=\"font-family: Courier New; font-size: 8pt;\">LinearizeSMIBGeneral </span>within <span style=\"font-family: Courier New; font-size: 8pt; color: #1c6cc8;\">SMIB_PSControl.Analysis.LinearAnalysis.BasicLinearization.LinearizeSMIBGeneral()</span></p>
</html>"));
        end LinearModelGeneral;
      end LinAtZero;

      package LinAfterDisturbance
        extends Modelica.Icons.ExamplesPackage;
        model NonlinModel_for_Linearization
          extends Modelica.Icons.Example;
          Modelica.Blocks.Interfaces.RealOutput Vt
            annotation (Placement(transformation(extent={{100,70},{120,90}}),
                iconTransformation(extent={{100,70},{120,90}})));
        public
          Modelica.Blocks.Interfaces.RealOutput Q
            annotation (Placement(transformation(extent={{100,-10},{120,10}}),
                iconTransformation(extent={{100,-10},{120,10}})));
          Modelica.Blocks.Interfaces.RealOutput P
            annotation (Placement(transformation(extent={{100,30},{120,50}}),
                iconTransformation(extent={{100,30},{120,50}})));
          Modelica.Blocks.Interfaces.RealOutput w
            annotation (Placement(transformation(extent={{100,-50},{120,-30}}),
                iconTransformation(extent={{100,-50},{120,-30}})));
          Modelica.Blocks.Interfaces.RealOutput delta
            annotation (Placement(transformation(extent={{100,-90},{120,-70}}),
                iconTransformation(extent={{100,-90},{120,-70}})));
          Modelica.Blocks.Interfaces.RealInput uPSS
            annotation (Placement(transformation(extent={{-140,40},{-100,80}})));
          Modelica.Blocks.Interfaces.RealInput uPm
            annotation (Placement(transformation(extent={{-140,-20},{-100,20}}),
                iconTransformation(extent={{-140,-20},{-100,20}})));
          Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
                transformation(extent={{-140,-60},{-100,-20}})));
          Interfaces.SMIB_AVR_PSS_wInput_wLineRmovaland4inputs
            sMIB_AVR_PSS_wInput_wLineRmovaland4inputs(t1=0.5)
            annotation (Placement(transformation(extent={{-40,-20},{40,60}})));
          Modelica.Blocks.Interfaces.RealInput uvsAVR annotation (Placement(
                transformation(extent={{-140,-100},{-100,-60}})));
          Modelica.Blocks.Routing.Multiplex4 Vs annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={-36,-70})));
          Modelica.Blocks.Interfaces.RealOutput BusVm[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={-36,-110})));
          Modelica.Blocks.Routing.Multiplex4 As annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={-12,-68})));
          Modelica.Blocks.Routing.Multiplex4 Ps annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={12,-68})));
          Modelica.Blocks.Routing.Multiplex4 Qs annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={36,-68})));
          Modelica.Blocks.Interfaces.RealOutput BusAn[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={-12,-110})));
          Modelica.Blocks.Interfaces.RealOutput Plines[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={12,-110})));
          Modelica.Blocks.Interfaces.RealOutput Qlines[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={38,-110})));
          Modelica.Blocks.Routing.Multiplex3 Adiffs annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={62,-70})));
          Modelica.Blocks.Interfaces.RealOutput BAnDiffs1toN[3]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=0,
                origin={70,-146})));
        equation
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPSS, uPSS)
            annotation (Line(points={{-44.2105,42.8571},{-69.5714,42.8571},{
                  -69.5714,60},{-120,60}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPm, uPm)
            annotation (Line(points={{-44.2105,20},{-84,20},{-84,0},{-120,0}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPload, uPload)
            annotation (Line(points={{-44.2105,2.85714},{-67.1428,2.85714},{
                  -67.1428,-40},{-120,-40}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Vt, Vt) annotation (
             Line(points={{42.1053,42.8571},{73.0714,42.8571},{73.0714,80},{110,
                  80}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.P, P) annotation (
              Line(points={{42.1053,31.4286},{72.0714,31.4286},{72.0714,40},{
                  110,40}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Q, Q) annotation (
              Line(points={{42.1053,20},{92,20},{92,0},{110,0}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.w, w) annotation (
              Line(points={{42.1053,8.57143},{88,8.57143},{88,-40},{110,-40}},
                color={0,0,127}));
          connect(uvsAVR, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uvsAVR)
            annotation (Line(points={{-120,-80},{-60,-80},{-60,-14.2857},{
                  -44.2105,-14.2857}}, color={0,0,127}));
          connect(Vs.y, BusVm) annotation (Line(points={{-36,-78.8},{-36,-110}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.delta, delta)
            annotation (Line(points={{42.1053,-2.85714},{84,-2.85714},{84,-80},
                  {110,-80}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm4, Vs.u4[1])
            annotation (Line(points={{-25.2632,-22.8571},{-25.2632,-51.4286},{
                  -43.2,-51.4286},{-43.2,-60.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm3, Vs.u3[1])
            annotation (Line(points={{-29.4737,-22.8571},{-29.4737,-51.4286},{
                  -38.4,-51.4286},{-38.4,-60.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm2, Vs.u2[1])
            annotation (Line(points={{-33.6842,-22.8571},{-33.6842,-51.4286},{
                  -33.6,-51.4286},{-33.6,-60.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm1, Vs.u1[1])
            annotation (Line(points={{-37.8947,-22.8571},{-37.8947,-51.4286},{
                  -28.8,-51.4286},{-28.8,-60.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva1, As.u1[1])
            annotation (Line(points={{-21.0526,-22.8571},{-21.0526,-50},{-4.8,
                  -50},{-4.8,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva2, As.u2[1])
            annotation (Line(points={{-16.8421,-22.8571},{-16.8421,-48},{-9.6,
                  -48},{-9.6,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva3, As.u3[1])
            annotation (Line(points={{-12.6316,-22.8571},{-12.6316,-52},{-14.4,
                  -52},{-14.4,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva4, As.u4[1])
            annotation (Line(points={{-8.42105,-22.8571},{-8.42105,-52},{-19.2,
                  -52},{-19.2,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline1, Ps.u1[1])
            annotation (Line(points={{-4.21053,-22.8571},{-4.21053,-50},{19.2,
                  -50},{19.2,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline2, Ps.u2[1])
            annotation (Line(points={{-1.77636e-15,-22.8571},{-1.77636e-15,-52},
                  {14.4,-52},{14.4,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline3, Ps.u3[1])
            annotation (Line(points={{4.21053,-22.8571},{4.21053,-54},{9.6,-54},
                  {9.6,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline4, Ps.u4[1])
            annotation (Line(points={{8.42105,-22.8571},{8.42105,-52},{4.8,-52},
                  {4.8,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline1, Qs.u1[1])
            annotation (Line(points={{12.6316,-22.8571},{12.6316,-50},{43.2,-50},
                  {43.2,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline2, Qs.u2[1])
            annotation (Line(points={{16.8421,-22.8571},{16.8421,-52},{38.4,-52},
                  {38.4,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline3, Qs.u3[1])
            annotation (Line(points={{21.0526,-22.8571},{21.0526,-48},{33.6,-48},
                  {33.6,-58.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline4, Qs.u4[1])
            annotation (Line(points={{25.2632,-22.8571},{25.2632,-52},{28.8,-52},
                  {28.8,-58.4}}, color={0,0,127}));
          connect(As.y, BusAn) annotation (Line(points={{-12,-76.8},{-12,-110}},
                color={0,0,127}));
          connect(Ps.y, Plines)
            annotation (Line(points={{12,-76.8},{12,-110}}, color={0,0,127}));
          connect(Qlines, Qs.y) annotation (Line(points={{38,-110},{38,-76.8},{
                  36,-76.8}}, color={0,0,127}));
          connect(Adiffs.y, BAnDiffs1toN) annotation (Line(points={{62,-81},{62,
                  -146},{70,-146}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to2, Adiffs.u1
            [1]) annotation (Line(points={{29.4737,-22.8571},{29.4737,-34},{69,
                  -34},{69,-58}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to3, Adiffs.u2
            [1]) annotation (Line(points={{33.6842,-22.8571},{62,-22.8571},{62,
                  -58}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to4, Adiffs.u3
            [1]) annotation (Line(points={{37.8947,-22.8571},{37.8947,-52},{55,
                  -52},{55,-58}}, color={0,0,127}));
          annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
                coordinateSystem(preserveAspectRatio=false, extent={{-100,-160},
                    {100,100}})));
        end NonlinModel_for_Linearization;

        model NonlinModel_for_NonlinExperiment
          extends Modelica.Icons.Example;
          Modelica.Blocks.Interfaces.RealOutput Vt
            annotation (Placement(transformation(extent={{98,68},{118,90}})));
        public
          Modelica.Blocks.Interfaces.RealOutput Q
            annotation (Placement(transformation(extent={{98,-10},{118,12}})));
          Modelica.Blocks.Interfaces.RealOutput P
            annotation (Placement(transformation(extent={{98,30},{118,52}})));
          Modelica.Blocks.Interfaces.RealOutput w
            annotation (Placement(transformation(extent={{98,-50},{118,-28}})));
          Modelica.Blocks.Interfaces.RealOutput delta
            annotation (Placement(transformation(extent={{98,-92},{118,-70}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-100,20},{-80,40}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-100,-10},{-80,10}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.05,
            offset=0,
            startTime=30.5)                                 annotation (Placement(
                transformation(extent={{-100,-40},{-80,-20}})));
          Interfaces.SMIB_AVR_PSS_wInput_wLineRmovaland4inputs
            sMIB_AVR_PSS_wInput_wLineRmovaland4inputs(t1=0.5)
            annotation (Placement(transformation(extent={{-20,-18},{8,10}})));
          Modelica.Blocks.Sources.Constant AVRvs_change(k=0)
                                                            annotation (Placement(
                transformation(extent={{-100,-80},{-80,-60}})));
          Modelica.Blocks.Routing.Multiplex4 Vs annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={-50,-232})));
          Modelica.Blocks.Routing.Multiplex4 As annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={-26,-230})));
          Modelica.Blocks.Routing.Multiplex4 Ps annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={-2,-230})));
          Modelica.Blocks.Routing.Multiplex4 Qs annotation (Placement(
                transformation(
                extent={{-8,-8},{8,8}},
                rotation=270,
                origin={22,-230})));
          Modelica.Blocks.Routing.Multiplex3 Adiffs annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={48,-232})));
          Modelica.Blocks.Interfaces.RealOutput BusVm[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={-50,-272})));
          Modelica.Blocks.Interfaces.RealOutput BusAn[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={-26,-272})));
          Modelica.Blocks.Interfaces.RealOutput Plines[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={-2,-272})));
          Modelica.Blocks.Interfaces.RealOutput Qlines[4]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={24,-272})));
          Modelica.Blocks.Interfaces.RealOutput BAnDiffs1toN[3]
            "Connector of Real output signals" annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=0,
                origin={70,-276})));
        equation
          connect(PSSchange.y, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPSS)
            annotation (Line(points={{-79,30},{-52,30},{-52,4},{-21.4737,4}},
                color={0,0,127}));
          connect(Pmchange.y, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPm)
            annotation (Line(points={{-79,0},{-50,0},{-50,-4},{-21.4737,-4}},
                color={0,0,127}));
          connect(Ploadchange.y, sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uPload)
            annotation (Line(points={{-79,-30},{-50,-30},{-50,-10},{-21.4737,
                  -10}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Vt, Vt) annotation (
             Line(points={{8.73684,4},{55.5,4},{55.5,79},{108,79}}, color={0,0,
                  127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.P, P) annotation (
              Line(points={{8.73684,0},{55.5,0},{55.5,41},{108,41}}, color={0,0,
                  127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Q, Q) annotation (
              Line(points={{8.73684,-4},{55.5,-4},{55.5,1},{108,1}}, color={0,0,
                  127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.w, w) annotation (
              Line(points={{8.73684,-8},{54.5,-8},{54.5,-39},{108,-39}}, color=
                  {0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.delta, delta)
            annotation (Line(points={{8.73684,-12},{56.5,-12},{56.5,-81},{108,
                  -81}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.uvsAVR,
            AVRvs_change.y) annotation (Line(points={{-21.4737,-16},{-40,-16},{
                  -40,-70},{-79,-70}}, color={0,0,127}));
          connect(Vs.y, BusVm) annotation (Line(points={{-50,-240.8},{-50,-272}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm4, Vs.u4[1])
            annotation (Line(points={{-14.8421,-19},{-14.8421,-187.429},{-57.2,
                  -187.429},{-57.2,-222.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm3, Vs.u3[1])
            annotation (Line(points={{-16.3158,-19},{-16.3158,-187.429},{-52.4,
                  -187.429},{-52.4,-222.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm2, Vs.u2[1])
            annotation (Line(points={{-17.7895,-19},{-17.7895,-187.429},{-47.6,
                  -187.429},{-47.6,-222.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvm1, Vs.u1[1])
            annotation (Line(points={{-19.2632,-19},{-19.2632,-187.429},{-42.8,
                  -187.429},{-42.8,-222.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva1, As.u1[1])
            annotation (Line(points={{-13.3684,-19},{-13.3684,-186},{-18.8,-186},
                  {-18.8,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva2, As.u2[1])
            annotation (Line(points={{-11.8947,-19},{-11.8947,-184},{-23.6,-184},
                  {-23.6,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva3, As.u3[1])
            annotation (Line(points={{-10.4211,-19},{-10.4211,-188},{-28.4,-188},
                  {-28.4,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bva4, As.u4[1])
            annotation (Line(points={{-8.94737,-19},{-8.94737,-188},{-33.2,-188},
                  {-33.2,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline1, Ps.u1[1])
            annotation (Line(points={{-7.47368,-19},{-7.47368,-186},{5.2,-186},
                  {5.2,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline2, Ps.u2[1])
            annotation (Line(points={{-6,-19},{-6,-188},{0.4,-188},{0.4,-220.4}},
                color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline3, Ps.u3[1])
            annotation (Line(points={{-4.52632,-19},{-4.52632,-190},{-4.4,-190},
                  {-4.4,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Pline4, Ps.u4[1])
            annotation (Line(points={{-3.05263,-19},{-3.05263,-188},{-9.2,-188},
                  {-9.2,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline1, Qs.u1[1])
            annotation (Line(points={{-1.57895,-19},{-1.57895,-186},{29.2,-186},
                  {29.2,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline2, Qs.u2[1])
            annotation (Line(points={{-0.105263,-19},{-0.105263,-188},{24.4,
                  -188},{24.4,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline3, Qs.u3[1])
            annotation (Line(points={{1.36842,-19},{1.36842,-184},{19.6,-184},{
                  19.6,-220.4}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Qline4, Qs.u4[1])
            annotation (Line(points={{2.84211,-19},{2.84211,-188},{14.8,-188},{
                  14.8,-220.4}}, color={0,0,127}));
          connect(As.y, BusAn) annotation (Line(points={{-26,-238.8},{-26,-272}},
                color={0,0,127}));
          connect(Ps.y, Plines)
            annotation (Line(points={{-2,-238.8},{-2,-272}}, color={0,0,127}));
          connect(Qlines, Qs.y) annotation (Line(points={{24,-272},{24,-238.8},
                  {22,-238.8}}, color={0,0,127}));
          connect(Adiffs.y, BAnDiffs1toN) annotation (Line(points={{48,-243},{
                  48,-276},{70,-276}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to2, Adiffs.u1
            [1]) annotation (Line(points={{4.31579,-19},{4.31579,-170},{55,-170},
                  {55,-220}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to3, Adiffs.u2
            [1]) annotation (Line(points={{5.78947,-19},{2,-19},{2,-220},{48,
                  -220}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.Bvadiff1to4, Adiffs.u3
            [1]) annotation (Line(points={{7.26316,-19},{7.26316,-188},{41,-188},
                  {41,-220}}, color={0,0,127}));
          annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
                coordinateSystem(preserveAspectRatio=false, extent={{-100,-300},
                    {100,100}})),
            experiment(
              StopTime=40,
              Tolerance=1e-06,
              __Dymola_Algorithm="Dassl"));
        end NonlinModel_for_NonlinExperiment;

        model LinearModelExperiment "Simulate the linearized model"
          extends Modelica.Icons.Example;
          extends
            SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
          inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
            annotation (Placement(transformation(extent={{72,70},{92,90}})));
          Modelica.Blocks.Routing.Multiplex4 multiplex4_1(n1=1, n2=1)
            annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
          Modelica_LinearSystems2.Controller.StateSpace stateSpace(system(
              A=[0.0,376.99111838904315,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0;
                  -0.0686617215242248,0.0,0.0,0.0,-0.13089553803908988,-0.007996158868440666,
                  0.0,0.0,0.0,0.0,0.0,0.0; -0.17084367949423743,0.0,-0.1249999999923592,
                  0.0,-0.17040348894923124,-0.0005205012335232465,0.0,0.0,
                  0.12499999998295178,0.0,0.0,0.0; -0.054929009801945934,0.0,
                  0.0,-1.0000000000890128,0.006479547869323119,-0.9628997950762316,
                  0.0,0.0,0.0,0.0,0.0,0.0; -2.249422089048476,0.0,
                  33.33333333129578,0.0,-35.57695963029725,-0.006853206950210671,
                  0.0,0.0,0.0,0.0,0.0,0.0; -0.31230970568104743,0.0,0.0,
                  14.285714286985895,0.03684074590905176,-19.760470755517055,
                  0.0,0.0,0.0,0.0,0.0,0.0; -11.015053434185218,0.0,0.0,0.0,
                  39.13100630743938,34.81062225996215,-66.66666666037753,0.0,
                  0.0,0.0,0.0,0.0; 0.0,0.0,0.0,0.0,0.0,0.0,0.0,-1.0,0.0,0.0,0.0,
                  0.0; 0.0,18999999.999782242,0.0,0.0,0.0,0.0,-1999999.9998113255,
                  10000.00082740371,-9999.999998636142,0.0,0.0,-18999999.999786716;
                  0.0,0.009499999999891122,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.0009999999999999983,
                  0.0,-0.009499999999893358; 0.0,0.009499999999891122,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0,0.0,-0.0009999999999999983,-0.009499999999893358;
                  0.0,0.7092198580775194,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.7092198580776865],
              B=[0.0,0.0,0.0,0.0; 0.0,0.14285715467719584,-0.04316835777729011,
                  0.0; 0.0,0.0,-0.06524164541943378,0.0; 0.0,0.0,
                  0.11073381100956681,0.0; 0.0,0.0,-0.8590073097280992,0.0; 0.0,
                  0.0,0.6295991505922416,0.0; 0.0,0.0,-2.8629247121140606,0.0;
                  0.0,0.0,0.0,0.0; 19000001.57206705,0.0,0.0,2000000.1654807418;
                  0.009500000786033523,0.0,0.0,0.0; 0.009500000786033523,0.0,
                  0.0,0.0; 0.7092199168371426,0.0,0.0,0.0],
              C=[-0.16522580151277824,0.0,0.0,0.0,0.5869650946115906,
                  0.5221593338994323,0.0,0.0,0.0,0.0,0.0,0.0;
                  0.5205641321750251,0.0,0.0,0.0,1.3973765863572156,
                  0.3037167771848266,0.0,0.0,0.0,0.0,0.0,0.0; 0.475728678419613,
                  0.0,0.0,0.0,0.9112481249714012,0.05799942913563993,0.0,0.0,
                  0.0,0.0,0.0,0.0; 0.0,0.9999999998893024,0.0,0.0,0.0,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0; 0.9999999998634491,0.0,0.0,0.0,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0,0.0; -0.16522580151277824,0.0,0.0,0.0,
                  0.5869650946115906,0.5221593338994323,0.0,0.0,0.0,0.0,0.0,0.0;
                  -0.2380119769112658,0.0,0.0,0.0,0.4187937135019326,
                  0.4960274594532118,0.0,0.0,0.0,0.0,0.0,0.0; 0.0,0.0,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0; -0.2401468119063274,0.0,0.0,
                  0.0,0.09970262897815439,0.3066851075856762,0.0,0.0,0.0,0.0,
                  0.0,0.0; 49.93342515829304,0.0,0.0,0.0,30.487241898783058,-33.02451389082256,
                  0.0,0.0,0.0,0.0,0.0,0.0; 42.09056557173591,0.0,0.0,0.0,
                  30.64011950471405,-24.871366757591574,0.0,0.0,0.0,0.0,0.0,0.0;
                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0;
                  17.396866559589107,0.0,0.0,0.0,19.8256335585288,-5.98111076074859,
                  0.0,0.0,0.0,0.0,0.0,0.0; 238.09499573237795,0.0,0.0,0.0,
                  447.43843774582666,23.8491521018521,0.0,0.0,0.0,0.0,0.0,0.0;
                  818.0226707328012,0.0,0.0,0.0,1575.5323996939374,
                  104.90957924781301,0.0,0.0,0.0,0.0,0.0,0.0; 835.9640231222115,
                  0.0,0.0,0.0,1568.0836231606652,81.99715491716861,0.0,0.0,0.0,
                  0.0,0.0,0.0; 5.381921629566201E-23,0.0,0.0,0.0,
                  7.187477745924726E-24,-3.6143339685564424E-23,0.0,0.0,0.0,0.0,
                  0.0,0.0; 133.6588434065159,0.0,0.0,0.0,563.5054719600828,
                  200.86521105086337,0.0,0.0,0.0,0.0,0.0,0.0; 477.7191919089208,
                  0.0,0.0,0.0,1981.37935449683,698.3072543573411,0.0,0.0,0.0,
                  0.0,0.0,0.0; -546.9805746873345,0.0,0.0,0.0,931.5920338055748,
                  1121.415495575262,0.0,0.0,0.0,0.0,0.0,0.0; -8.036847787328406E-23,
                  0.0,0.0,0.0,-1.6922587313141753E-24,-1.2670734948503991E-23,
                  0.0,0.0,0.0,0.0,0.0,0.0; 7.842859586557134,0.0,0.0,0.0,-0.15287760593099217,
                  -8.15314713323099,0.0,0.0,0.0,0.0,0.0,0.0; 49.93342515829304,
                  0.0,0.0,0.0,30.487241898783058,-33.02451389082256,0.0,0.0,0.0,
                  0.0,0.0,0.0; 32.536558598703934,0.0,0.0,0.0,
                  10.661608340254258,-27.043403130073973,0.0,0.0,0.0,0.0,0.0,
                  0.0],
              D=[0.0,0.0,-0.042943870681710905,0.0; 0.0,0.0,0.10523992788336045,
                  0.0; 0.0,0.0,0.30002678119700477,0.0; 0.0,0.0,0.0,0.0; 0.0,
                  0.0,0.0,0.0; 0.0,0.0,-0.042943870681710905,0.0; 0.0,0.0,-0.053600124338970545,
                  0.0; 0.0,0.0,0.0,0.0; 0.0,0.0,-0.003282707439211663,0.0; 0.0,
                  0.0,-4.124160568608204,0.0; 0.0,0.0,-7.714881178344512,0.0;
                  0.0,0.0,0.0,0.0; 0.0,0.0,-20.61482007320592,0.0; 0.0,0.0,-88.65583822625922,
                  0.0; 0.0,0.0,754.7155291831587,0.0; 0.0,0.0,-1465.0388493464561,
                  0.0; 0.0,0.0,1.2225480381593275E-21,0.0; 0.0,0.0,-101.05819114869519,
                  0.0; 0.0,0.0,95.8439727583027,0.0; 0.0,0.0,-550.6037723534973,
                  0.0; 0.0,0.0,3.1139425251908828E-21,0.0; 0.0,0.0,
                  3.5907206097363082,0.0; 0.0,0.0,-4.124160568608204,0.0; 0.0,
                  0.0,16.490659504597716,0.0],
              yNames={"Vt","Q","P","w","delta","BusVm[1]","BusVm[2]","BusVm[3]",
                  "BusVm[4]","BusAn[1]","BusAn[2]","BusAn[3]","BusAn[4]",
                  "Plines[1]","Plines[2]","Plines[3]","Plines[4]","Qlines[1]",
                  "Qlines[2]","Qlines[3]","Qlines[4]","BAnDiffs1toN[1]",
                  "BAnDiffs1toN[2]","BAnDiffs1toN[3]"},
              xNames={
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.delta",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.w",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.e1q",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.e1d",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.e2q",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.machine.e2d",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.avr.vm",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.avr.vr",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.avr.vf1",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.pss.imLeadLag.TF.x_scaled[1]",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.pss.imLeadLag1.TF.x_scaled[1]",
                  "sMIB_AVR_PSS_wInput_wLineRmovaland4inputs.G1.pss.derivativeLag.TF.x_scaled[1]"},
              uNames={"uPSS","uPm","uPload","uvsAVR"}),
                                      blockType=Modelica_LinearSystems2.Controller.Types.BlockTypeWithGlobalDefault.Continuous)
            annotation (Placement(transformation(extent={{-36,-10},{-16,10}})));

          Modelica.Blocks.Math.Add addy[24]
            annotation (Placement(transformation(extent={{6,-16},{26,4}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-122,20},{-102,40}})));
          Modelica.Blocks.Sources.Constant y0_initial[24](k={0.999999582767487,
                0.200358614325523,0.899999439716339,1,1.29897058010101,
                0.999999582767487,0.979295551776886,1,0.978660404682159,
                24.9878368377686,17.0641422271729,0,7.98409032821655,
                196.29345703125,722.09130859375,648.975646972656,
                1079.61401367188,15.5986824035645,60.3069686889648,-54.4150314331055,
                85.7927551269531,7.92369508743286,24.9878368377686,
                17.0037479400635}) annotation (Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={0,-52})));
          Modelica.Blocks.Sources.Constant AVRvs_change(k=0)
                                                            annotation (Placement(
                transformation(extent={{-122,-80},{-102,-60}})));
          Modelica.Blocks.Routing.DeMultiplex y(n=24)
            annotation (Placement(transformation(extent={{40,-16},{60,4}})));
          Modelica.Blocks.Interfaces.RealOutput BusVm[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={30,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput BusAn[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={50,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput Plines[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={70,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput Qlines[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={92,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput BAnDiffs1toN[3] annotation (
              Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={110,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.05,
            offset=0,
            startTime=30.5)                                 annotation (Placement(
                transformation(extent={{-120,-40},{-100,-20}})));
        equation
          connect(multiplex4_1.y, stateSpace.u)
            annotation (Line(points={{-59,0},{-38,0}}, color={0,0,127}));
          connect(stateSpace.y, addy.u1)
            annotation (Line(points={{-15,0},{4,0}}, color={0,0,127}));
          connect(Pmchange.y,multiplex4_1. u2[1]) annotation (Line(points={{-99,0},
                  {-92,0},{-92,3},{-82,3}},color={0,0,127}));
          connect(PSSchange.y,multiplex4_1. u1[1]) annotation (Line(points={{-101,30},{-92,
                  30},{-92,9},{-82,9}},               color={0,0,127}));
          connect(y0_initial.y, addy.u2)
            annotation (Line(points={{0,-41},{0,-12},{4,-12}}, color={0,0,127}));
          connect(AVRvs_change.y, multiplex4_1.u4[1]) annotation (Line(points={{-101,-70},
                  {-92,-70},{-92,-9},{-82,-9}}, color={0,0,127}));
          connect(addy.y, y.u)
            annotation (Line(points={{27,-6},{38,-6}}, color={0,0,127}));
          connect(Vt, y.y[1]) annotation (Line(points={{150,80},{110,80},{110,
                  0.708333},{60,0.708333}}, color={0,0,127}));
          connect(Q, y.y[2]) annotation (Line(points={{150,0},{112,0},{112,
                  0.125},{60,0.125}}, color={0,0,127}));
          connect(P, y.y[3]) annotation (Line(points={{150,40},{112,40},{112,
                  -0.458333},{60,-0.458333}}, color={0,0,127}));
          connect(w, y.y[4]) annotation (Line(points={{150,-40},{126,-40},{126,
                  -1.04167},{60,-1.04167}}, color={0,0,127}));
          connect(delta, y.y[5]) annotation (Line(points={{150,-80},{131,-80},{
                  131,-1.625},{60,-1.625}}, color={0,0,127}));
          connect(y.y[6:9], BusVm) annotation (Line(points={{60,-3.95833},{60,-26},
                  {30,-26},{30,-50}}, color={0,0,127}));
          connect(BusAn, y.y[10:13]) annotation (Line(points={{50,-50},{50,-28},
                  {64,-28},{64,-6.29167},{60,-6.29167}}, color={0,0,127}));
          connect(y.y[14:17], Plines) annotation (Line(points={{60,-8.625},{70,
                  -8.625},{70,-50}}, color={0,0,127}));
          connect(y.y[18:21], Qlines) annotation (Line(points={{60,-10.9583},{
                  76,-10.9583},{76,-8},{92,-8},{92,-50}}, color={0,0,127}));
          connect(BAnDiffs1toN, y.y[22:24]) annotation (Line(points={{110,-50},
                  {110,-12.7083},{60,-12.7083}}, color={0,0,127}));
          connect(Ploadchange.y, multiplex4_1.u3[1]) annotation (Line(points={{
                  -99,-30},{-98,-30},{-98,-3},{-82,-3}}, color={0,0,127}));
          annotation (
            Icon(coordinateSystem(preserveAspectRatio=false)),
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{140,100}}),                           graphics={
                Text(
                  extent={{-58,24},{-44,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="du",
                  fontSize=24),
                Text(
                  extent={{-38,-18},{-22,-38}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="y0",
                  fontSize=24),
                Text(
                  extent={{40,20},{50,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="y"),
                Text(
                  extent={{-12,22},{2,6}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="dy"),
                Text(
                  extent={{-80,80},{0,60}},
                  lineColor={85,170,255},
                  fillPattern=FillPattern.HorizontalCylinder,
                  fillColor={28,108,200},
                  horizontalAlignment=TextAlignment.Left,
                  textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
                Line(
                  points={{-4,64},{24,44},{18,14}},
                  color={0,0,255},
                  thickness=0.5,
                  arrow={Arrow.None,Arrow.Filled},
                  smooth=Smooth.Bezier)}),
            experiment(
              StopTime=40,
              __Dymola_NumberOfIntervals=10000,
              Tolerance=1e-06,
              __Dymola_fixedstepsize=0.01,
              __Dymola_Algorithm="Dassl"),
            __Dymola_Commands(file="MosScripts/linearize_and_compare.mos"
                "linearize_and_compare", file=
                  "MosScripts/linearize_and_compare.mos"
                "linearize_and_compare"));
        end LinearModelExperiment;

        function LinearizeSimple
          // Import things needed for the calculations
          import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
          // Declare outputs to display
          output Real A[:,:] "A-matrix";
          output Real B[:,:] "B-matrix";
          output Real C[:,:] "C-matrix";
          output Real D[:,:] "D-matrix";
          output String inputNames[:] "Modelica names of inputs";
          output String outputNames[:] "Modelica names of outputs";
          output String stateNames[:] "Modelica names of states";
          // Declare reconfigurable inputs
          input Modelica.SIunits.Time tlin = 30 "t for model linearization";
          input Modelica.SIunits.Time tsim = 30 "Simulation time";
          input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbanceW4inputsAndManyOutputs.LinAfterDisturbance.NonlinModel_for_Linearization";
          input String pathToNonlinearExperiment = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbanceW4inputsAndManyOutputs.LinAfterDisturbance.NonlinModel_for_NonlinExperiment";
        algorithm
          // Compute and display the ABCD matrices, etc
          Modelica.Utilities.Streams.print("Linearized Model");
          (A,B,C,D,inputNames,outputNames,stateNames) :=
            Modelica_LinearSystems2.Utilities.Import.linearize(
            pathToNonlinearPlantModel,tlin);
          nx := size(A, 1); //number of states
          Modelica.Utilities.Streams.print("Number of states" + String(nx));
          // Now we want to extract the initial value of the outputs to use it in our
          // linear model response
          Modelica.Utilities.Streams.print("Simulating nonlinear model");
          simulateModel(
            pathToNonlinearExperiment,
            stopTime=tsim,
            numberOfIntervals=1000,
            resultFile="res_nl");
           ny := size(C, 1);
           y0 := DymolaCommands.Trajectories.readTrajectory(
             "res_nl.mat",
             {outputNames[i] for i in 1:ny},
             DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat"));
           DataFiles.writeMATmatrix(
             "MyData.mat",
             "y0",
             [y0[1:ny,1]],
             append=true);
          // Print y0's first values which is needed for the linear response model
          y0out := y0[:,1]; // we only want the first few elements
          Modelica.Utilities.Streams.print("y0 =");
          Modelica.Math.Vectors.toString(y0out);
          annotation(__Dymola_interactive=true);
        end LinearizeSimple;

        function LinearizeAndCompare
          "Linearizes the model at initialization and after a disturbance at a given time"
          // See the Documentation for an explanation of the goals.
          // IMPORTING FUNCTIONS
          // Import things needed for the calculations
          import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
          // OUTPUTS OF THEFUNCTION - FOR DISPLAY
          // Declare outputs to display
          output Real A[:,:] "A-matrix";
          output Real B[:,:] "B-matrix";
          output Real C[:,:] "C-matrix";
          output Real D[:,:] "D-matrix";
          output String inputNames[:] "Modelica names of inputs";
          output String outputNames[:] "Modelica names of outputs";
          output String stateNames[:] "Modelica names of states";
          output Real y0out[:] "Initial value of the output variables";
          // INPUTS TO THE FUNCTION
          // Declare reconfigurable simulation parameters
          input Modelica.SIunits.Time tlin = 30 "t for model linearization";
          input Modelica.SIunits.Time tsim = 40 "Simulation time";
          input Real numberOfIntervalsin=10000 "No. of intervals";
          // Use this for Case A
          //input String method = "Rkfix4" "Solver";
          input String methodin = "DASSL" "Solver";
          input Real fixedstepsizein= 1e-12 "Time step - needed only for fixed time step solvers";
          //
          // DEFINING THE NONLINEAR PLANT, NONLINEAR EXPERIMENT, AND LINEAR EXPERIMENT MODELS
          //
          // 1) NONLINEAR PLANT:
          // This is the model that will be linearized, i.e. the nonlinear plant model
           input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbanceW4inputsAndManyOutputs.LinAfterDisturbance.NonlinModel_for_Linearization" "Nonlinear plant model";
          //
          //
          // 2) NONLINEAR EXPERIMENT: this is a model which applies a change to the input of the nonlinear model.
          // It must match the nonlinar plant above.
          // This model will be simulated, and the simulation results will be compared to the simulation of the corresponding linearized model.
          input String pathToNonlinearExperiment= "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbanceW4inputsAndManyOutputs.LinAfterDisturbance.NonlinModel_for_NonlinExperiment" "Nonlinear experiment model";
          //
          //
          // 3) LINEAR EXPERIMENT: this is a template that can be used for all three cases, so it is not necessary to create other cases here
          input String pathToLinearExperiment = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbanceW4inputsAndManyOutputs.LinAfterDisturbance.LinearModelGeneral";

        algorithm
          // Compute and display the ABCD matrices, etc
          (A,B,C,D,inputNames,outputNames,stateNames) :=
            Modelica_LinearSystems2.Utilities.Import.linearize(
            pathToNonlinearPlantModel,tlin);
          // LINEARIZE plant model at t_lin
          // This is the same as above, however, it stores it in a StateSpace object
          ss := Modelica_LinearSystems2.ModelAnalysis.Linearize(
            pathToNonlinearPlantModel, simulationSetup=
            Modelica_LinearSystems2.Records.SimulationOptionsForLinearization(
            linearizeAtInitial=false, t_linearize=tlin));
          // PRINT linear system
          Modelica.Utilities.Streams.print(String(ss));
          // SAVE the data in a mat file
          DataFiles.writeMATmatrix(
            "MyData.mat",
            "ABCD",
            [ss.A, ss.B; ss.C, ss.D],
            append=false);
          nx := size(ss.A, 1);
          DataFiles.writeMATmatrix(
            "MyData.mat",
            "nx",
            [nx],
            append=true);
          Modelica.Utilities.Streams.print("Simulating nonlinear model");
          Modelica.Utilities.Streams.print("Simulating nonlinear model for comparison, for the entire length of the simulation; i.e. before and after tlin if tlin is not at zero or tsim");
          simulateModel(
            pathToNonlinearExperiment,
            stopTime=tsim,
            numberOfIntervals=numberOfIntervalsin, method = methodin, fixedstepsize=fixedstepsizein,
            resultFile="res_nl");
           ny := size(ss.C, 1);
           // we need to simulate the model again to extract the initial values at time tlin
           // this is to extract the initial values of the linearized model before we apply a second perturbaction
          Modelica.Utilities.Streams.print("Simulating nonlinear model to extract initial output values until tlin");
          simulateModel(
            pathToNonlinearExperiment,
            stopTime=tlin,
            numberOfIntervals=numberOfIntervalsin, method = methodin, fixedstepsize=fixedstepsizein,
            resultFile="res_nl_predist");
           ny := size(ss.C, 1);
           y0len :=DymolaCommands.Trajectories.readTrajectorySize("res_nl_predist.mat"); // here we want to find the last element at time tlin
           y0 := DymolaCommands.Trajectories.readTrajectory(
             "res_nl_predist.mat",
             {ss.yNames[i] for i in 1:ny},y0len);
           DataFiles.writeMATmatrix(
             "MyData.mat",
             "y0",
             [y0[1:ny,y0len]],
             append=true);    // the entry y0length points out to the location in the vector to the point of trajectory that we want, so here we take
                               // all the values in the rows, for the last column (last element at time tlin)
          // Print y0's first values which is needed for the linear response model
          y0out := y0[:,y0len]; // we only want the elements at the time tlin
          Modelica.Utilities.Streams.print("y0 =");
          Modelica.Math.Vectors.toString(y0out);
          //
          // We now simulate the linear model, which requires y0
          Modelica.Utilities.Streams.print("Simulating linear model");
          simulateModel(
            pathToLinearExperiment,
            stopTime=tsim,
            numberOfIntervals=numberOfIntervalsin, method = methodin, fixedstepsize=fixedstepsizein,
            resultFile="res_lin");
            // Plot
        removePlots(true);
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Vt"}, range={0.0, 20.0, 0.998, 1.002}, grid=true, filename="res_nl.mat", colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Q"}, range={0.0, 20.0, 0.18, 0.21}, grid=true, subPlot=102, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"P"}, range={0.0, 20.0, 0.86, 0.94}, grid=true, subPlot=103, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"w"}, range={0.0, 20.0, 0.9996, 1.0004}, grid=true, subPlot=104, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"delta"}, range={0.0, 20.0, 1.27, 1.3}, grid=true, subPlot=105, colors={{28,108,200}}, displayUnits={"rad"});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Vt"}, range={0.0, 20.0, 0.998, 1.002}, erase=false, grid=true, filename="res_lin.mat", colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"Q"}, range={0.0, 20.0, 0.18, 0.21}, erase=false, grid=true, subPlot=102, colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"P"}, range={0.0, 20.0, 0.86, 0.94}, erase=false, grid=true, subPlot=103, colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"w"}, range={0.0, 20.0, 0.9996, 1.0004}, erase=false, grid=true, subPlot=104, colors={{238,46,47}});
        createPlot(id=1, position={-2, 1, 584, 782}, y={"delta"}, range={0.0, 20.0, 1.27, 1.3}, erase=false, grid=true, subPlot=105, colors={{238,46,47}});

          annotation(__Dymola_interactive=true, Documentation(info="<html>
<p>This function linearizes the model at two different times, initialization and at a user provided time.</p>
</html>"));
        end LinearizeAndCompare;

        model LinearModelGeneral
          "Simulate the linearized SMIB model obtained by running the function LinearizeSMIB."
          extends SMIBPS_IdControl.Utilities.Icons.FunctionDependentExample;
          extends
            SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
          // The following definitions are very important to couple the linear model
          // to the linearization of the nonlinear model and the simulation
          parameter Real[:] y0=vector(DataFiles.readMATmatrix("MyData.mat", "y0")) annotation (Evaluate=false);
          // The following has to be imported in order to be able to interpret and manipulate the StateSpace types
          import Modelica_LinearSystems2.StateSpace;
          parameter StateSpace ss=StateSpace.Import.fromFile("MyData.mat", "ABCD");
          parameter Integer ny=size(ss.C, 1);
          inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
            annotation (Placement(transformation(extent={{60,60},{80,80}})));
          Modelica.Blocks.Routing.Multiplex4 multiplex4_1(n1=1, n2=1)
            annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
          Modelica.Blocks.Math.Add addy[ny]
            annotation (Placement(transformation(extent={{6,-16},{26,4}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-122,-10},{-102,10}})));
          Modelica.Blocks.Sources.Constant y0_initial[ny](k=y0)      annotation (
              Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={0,-32})));
          Modelica_LinearSystems2.Controller.StateSpace stateSpace(system=ss)
            annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-120,20},{-100,40}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.05,
            offset=0,
            startTime=30.5)                                 annotation (Placement(
                transformation(extent={{-120,-40},{-100,-20}})));
          Modelica.Blocks.Sources.Constant AVRvs_change(k=0)
                                                            annotation (Placement(
                transformation(extent={{-120,-80},{-100,-60}})));
          Modelica.Blocks.Routing.DeMultiplex y(n=24)
            annotation (Placement(transformation(extent={{38,-16},{58,4}})));
          Modelica.Blocks.Interfaces.RealOutput BusVm[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={30,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput BusAn[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={50,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput Plines[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={70,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput Qlines[4] annotation (Placement(
                transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={92,-50}), iconTransformation(extent={{140,30},{160,50}})));
          Modelica.Blocks.Interfaces.RealOutput BAnDiffs1toN[3] annotation (
              Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=270,
                origin={110,-50}), iconTransformation(extent={{140,30},{160,50}})));
        equation
          connect(Pmchange.y,multiplex4_1. u2[1]) annotation (Line(points={{-101,0},
                  {-92,0},{-92,3},{-82,3}},color={0,0,127}));
          connect(y0_initial.y, addy.u2)
            annotation (Line(points={{0,-21},{0,-12},{4,-12}}, color={0,0,127}));
          connect(multiplex4_1.y, stateSpace.u)
            annotation (Line(points={{-59,0},{-42,0}}, color={0,0,127}));
          connect(stateSpace.y, addy.u1)
            annotation (Line(points={{-19,0},{4,0}}, color={0,0,127}));
          connect(PSSchange.y,multiplex4_1. u1[1]) annotation (Line(points={{-99,30},
                  {-92,30},{-92,9},{-82,9}},         color={0,0,127}));
          connect(Ploadchange.y,multiplex4_1. u3[1]) annotation (Line(points={{-99,-30},
                  {-90,-30},{-90,-3},{-82,-3}},          color={0,0,127}));
          connect(AVRvs_change.y, multiplex4_1.u4[1]) annotation (Line(points={
                  {-99,-70},{-86,-70},{-86,-9},{-82,-9}}, color={0,0,127}));
          connect(addy.y,y. u)
            annotation (Line(points={{27,-6},{36,-6}}, color={0,0,127}));
          connect(Vt, y.y[1]) annotation (Line(points={{150,80},{104,80},{104,
                  0.708333},{58,0.708333}}, color={0,0,127}));
          connect(Q, y.y[2]) annotation (Line(points={{150,0},{106,0},{106,
                  0.125},{58,0.125}}, color={0,0,127}));
          connect(P, y.y[3]) annotation (Line(points={{150,40},{106,40},{106,
                  -0.458333},{58,-0.458333}}, color={0,0,127}));
          connect(w, y.y[4]) annotation (Line(points={{150,-40},{132,-40},{132,
                  -1.04167},{58,-1.04167}}, color={0,0,127}));
          connect(delta, y.y[5]) annotation (Line(points={{150,-80},{134,-80},{
                  134,-1.625},{58,-1.625}}, color={0,0,127}));
          connect(BusVm, y.y[6:9]) annotation (Line(points={{30,-50},{30,-18},{
                  64,-18},{64,-3.95833},{58,-3.95833}}, color={0,0,127}));
          connect(BusAn, y.y[10:13]) annotation (Line(points={{50,-50},{50,-20},
                  {66,-20},{66,-6.29167},{58,-6.29167}}, color={0,0,127}));
          connect(Plines, y.y[14:17]) annotation (Line(points={{70,-50},{70,
                  -8.625},{58,-8.625}}, color={0,0,127}));
          connect(Qlines, y.y[18:21]) annotation (Line(points={{92,-50},{92,
                  -10.9583},{58,-10.9583}}, color={0,0,127}));
          connect(BAnDiffs1toN, y.y[22:24]) annotation (Line(points={{110,-50},
                  {110,-12.7083},{58,-12.7083}}, color={0,0,127}));
          annotation (
            Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                    {140,100}})),
            Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
                    -100},{140,100}}),                           graphics={
                Text(
                  extent={{-58,24},{-44,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="du",
                  fontSize=24),
                Text(
                  extent={{-12,20},{-2,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="dy",
                  fontSize=24),
                Text(
                  extent={{-38,-18},{-22,-38}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  textString="y0",
                  fontSize=24),
                Text(
                  extent={{40,20},{50,4}},
                  lineColor={238,46,47},
                  fillPattern=FillPattern.VerticalCylinder,
                  fillColor={255,0,0},
                  fontSize=24,
                  textString="y"),
                Text(
                  extent={{-80,80},{0,60}},
                  lineColor={85,170,255},
                  fillPattern=FillPattern.HorizontalCylinder,
                  fillColor={28,108,200},
                  horizontalAlignment=TextAlignment.Left,
                  textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
                Text(
                  extent={{-80,-80},{80,-100}},
                  lineColor={85,170,255},
                  fillPattern=FillPattern.HorizontalCylinder,
                  fillColor={28,108,200},
                  horizontalAlignment=TextAlignment.Left,
                  textString="Notice the change in the order of the outputs w.r.t. the nonlinear model.
They have to be rearranged based on the order provided by the linearization function."),
                Line(
                  points={{66,-86},{128,-82},{140,-8}},
                  color={0,0,255},
                  thickness=0.5,
                  arrow={Arrow.None,Arrow.Filled},
                  smooth=Smooth.Bezier),
                Line(
                  points={{-4,68},{24,48},{18,18}},
                  color={0,0,255},
                  thickness=0.5,
                  arrow={Arrow.None,Arrow.Filled},
                  smooth=Smooth.Bezier)}),
            experiment(
              StopTime=15,
              __Dymola_NumberOfIntervals=1000,
              __Dymola_Algorithm="Dassl"),
            Documentation(info="<html>
<p>DO NOT try to run this model on it&apos;s own! </p>
<p>Models with this icon will not simulate on their own, instead they work together with a function that populates certain parameters in the model and perform other operations.</p>
<p><br>See the associated function to run:<span style=\"color: #1c6cc8;\"> <span style=\"font-family: Courier New; font-size: 8pt;\">LinearizeSMIBGeneral </span>within <span style=\"font-family: Courier New; font-size: 8pt; color: #1c6cc8;\">SMIB_PSControl.Analysis.LinearAnalysis.BasicLinearization.LinearizeSMIBGeneral()</span></p>
</html>"));
        end LinearModelGeneral;
      end LinAfterDisturbance;
    end LinearizeAfterDisturbanceW4inputsAndManyOutputs;
  end LinearAnalysis;

  package ClassicalControlDesign "Control design using"
    extends Modelica.Icons.ExamplesPackage;

    package Interfaces
      extends Modelica.Icons.InterfacesPackage;
      model SMIB_AVR_wLineRmovaland4inputs
        extends SMIBPS_IdControl.BaseModelsPartial.OutputInterfaces.OutputsInterfaceBusVoltagesBranchPowers_noInfiniteBus;
        extends SMIBPS_IdControl.BaseModelsPartial.OutputInterfaces.OutputsInterfaceBig;
        extends BaseModelsPartial.BaseNetwork.SMIB_Partial(powerFlow_Data(
            redeclare record Bus = PF_Data.Bus_Data.PF_Bus_5,
            redeclare record Loads = PF_Data.Loads_Data.PF_Loads_5,
            redeclare record Trafos = PF_Data.Trafos_Data.PF_Trafos_5,
            redeclare record Machines = PF_Data.Machines_Data.PF_Machines_5),
          line_1(X=3.25),
          line_2(t1=Modelica.Constants.inf),
          fault(t1=Modelica.Constants.inf, t2=Modelica.Constants.inf));
        import Modelica.Constants.pi;
        BaseModelsPartial.BasePlants.Generator_AVR_wInputs      G1(
          V_0=powerFlow_Data.bus.V1,
          P_0=powerFlow_Data.machines.PG1,
          Q_0=powerFlow_Data.machines.QG1,
          angle_0=powerFlow_Data.bus.A1)
          annotation (Placement(transformation(extent={{-140,-20},{-100,20}})));
        Modelica.Blocks.Interfaces.RealInput uPm
          annotation (Placement(transformation(extent={{-240,-20},{-200,20}})));
        Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
              transformation(extent={{-240,-180},{-200,-140}}),
              iconTransformation(extent={{-240,-180},{-200,-140}})));

        OpenIPSL.Electrical.Branches.PwLine line_4(
          R=0,
          G=0,
          B=0,
          X=3.25/5.5,
          t1=t1,
          t2=t2,
          opening=opening)
                 annotation (Placement(transformation(extent={{22,2},{40,14}})));
        parameter Modelica.SIunits.Time t1=Modelica.Constants.inf "Time of line removal"
          annotation (Dialog(group="Line Removal Parameters"));
        parameter Modelica.SIunits.Time t2=Modelica.Constants.inf
          "Line re-insertion time"     annotation (Dialog(group="Line Removal Parameters"));
        parameter Integer opening=1
          "Type of opening (1: removes both ends at same time, 2: removes sending end, 3: removes receiving end)"     annotation (Dialog(group="Line Removal Parameters"));
        Modelica.Blocks.Interfaces.RealInput uvsAVR
          annotation (Placement(transformation(extent={{-240,140},{-200,180}})));
      protected
        parameter Real S_b=SysData.S_b;
      equation
        w = G1.machine.w;
        delta = G1.machine.delta;
        Vt = G1.machine.v;
        P = G1.machine.P;
        Q = G1.machine.Q;
      //   // Assignment of outputs for the network voltages and powers
         Bvm1 = B1.V;
         Bva1 = B1.angle;
         Bvm2 = B2.V;
         Bva2 = B2.angle;
         Bvm4 = B4.V;
         Bva4 = B4.angle;
         Pline1 = line_1.P12;
         Qline1 = line_1.Q12;
         Pline2 = line_2.P12;
         Qline2 = line_2.Q12;
         Pline3 = line_3.P12;
         Qline3 = line_3.Q12;
         Pline4 = line_4.P12;
         Qline4 = line_4.Q12;
         Bvadiff1to2 = B1.angle - B2.angle;
         Bvadiff1to3 = B1.angle - B3.angle;
         Bvadiff1to4 = B1.angle - B4.angle;
        //
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-98,0},{-80,0}}, color={0,0,255}));
        connect(line_4.n, line_1.n)
          annotation (Line(points={{39.1,8},{39.1,20}}, color={0,0,255}));
        connect(line_4.p, line_1.p)
          annotation (Line(points={{22.9,8},{22.9,20}}, color={0,0,255}));
        connect(uvsAVR, G1.uVs) annotation (Line(points={{-220,160},{-182,160},{-182,12},
                {-144.4,12}},color={0,0,127}));
        connect(uPm, G1.pm) annotation (Line(points={{-220,0},{-182,0},{-182,-12},{-144,
                -12}},color={0,0,127}));
        connect(load_ExtInput.u, uPload) annotation (Line(points={{17.14,-66.7},{-194,
                -66.7},{-194,-160},{-220,-160}}, color={0,0,127}));
        annotation (
          Diagram(coordinateSystem(extent={{-200,-200},{200,200}}), graphics={
                Rectangle(
                extent={{12,28},{50,0}},
                lineColor={238,46,47},
                fillColor={244,125,35},
                fillPattern=FillPattern.None,
                lineThickness=1)}),
          Icon(coordinateSystem(extent={{-200,-200},{200,200}}), graphics={
              Rectangle(extent={{-200,200},{200,-200}}, lineColor={28,108,200}),
              Text(
                extent={{-200,40},{200,-62}},
                lineColor={28,108,200},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="Nonlinear
Model
with AVR

"),           Text(
                extent={{-200,180},{200,140}},
                lineColor={238,46,47},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="%name
")}),     experiment(
            StopTime=20,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
</html>"));
      end SMIB_AVR_wLineRmovaland4inputs;
    end Interfaces;

    package RootLocus
      extends Modelica.Icons.ExamplesPackage;
      package Design
        extends Modelica.Icons.ExamplesPackage;
        package ModelVariants
          extends Modelica.Icons.VariantsPackage;
          model NonlinearModelforSimulation_AVR
            extends Modelica.Icons.Example;
            Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
                  transformation(
                  extent={{-10,-10},{10,10}},
                  rotation=0,
                  origin={-152,0})));
            Modelica.Blocks.Sources.Constant Pmchange(k=0) annotation (Placement(
                  transformation(extent={{-200,-10},{-180,10}})));
            Modelica.Blocks.Sources.Constant Ploadchange(k=0) annotation (Placement(
                  transformation(extent={{-200,-120},{-180,-100}})));
            Modelica.Blocks.Math.Gain uPloadInputGain(k=1) annotation (Placement(
                  transformation(
                  extent={{-10,-10},{10,10}},
                  rotation=0,
                  origin={-152,-110})));
            Modelica.Blocks.Sources.Step     AVRvs_change(height=1, startTime=1)
                                                              annotation (Placement(
                  transformation(extent={{-200,104},{-180,124}})));
            Modelica.Blocks.Math.Gain uPloadInputGain1(k=1)
                                                           annotation (Placement(
                  transformation(
                  extent={{-10,-10},{10,10}},
                  rotation=0,
                  origin={-152,114})));
            Interfaces.SMIB_AVR_wLineRmovaland4inputs
              sMIB_AVR_wLineRmovaland4inputs annotation (Placement(
                  transformation(extent={{-100,-138},{180,142}})));
          equation
            connect(Pmchange.y,pmInputGain. u)
              annotation (Line(points={{-179,0},{-164,0}},   color={0,0,127}));
            connect(Ploadchange.y,uPloadInputGain. u)
              annotation (Line(points={{-179,-110},{-164,-110}},
                                                             color={0,0,127}));
            connect(AVRvs_change.y,uPloadInputGain1. u)
              annotation (Line(points={{-179,114},{-164,114}}, color={0,0,127}));
            connect(uPloadInputGain1.y, sMIB_AVR_wLineRmovaland4inputs.uvsAVR)
              annotation (Line(points={{-141,114},{-114,114}}, color={0,0,127}));
            connect(pmInputGain.y, sMIB_AVR_wLineRmovaland4inputs.uPm)
              annotation (Line(points={{-141,0},{-134,0},{-134,2},{-114,2}},
                  color={0,0,127}));
            connect(uPloadInputGain.y, sMIB_AVR_wLineRmovaland4inputs.uPload)
              annotation (Line(points={{-141,-110},{-114,-110}}, color={0,0,127}));
            annotation (Icon(coordinateSystem(extent={{-200,-200},{200,200}})), Diagram(
                  coordinateSystem(extent={{-200,-200},{200,200}})));
          end NonlinearModelforSimulation_AVR;

          model NonlinearModelforDesign_AVR
            extends Modelica.Icons.Example;
            Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
                  transformation(
                  extent={{-10,-10},{10,10}},
                  rotation=0,
                  origin={-152,0})));
            Modelica.Blocks.Sources.Constant Pmchange(k=0) annotation (Placement(
                  transformation(extent={{-200,-10},{-180,10}})));
            Modelica.Blocks.Sources.Constant Ploadchange(k=0) annotation (Placement(
                  transformation(extent={{-200,-120},{-180,-100}})));
            Modelica.Blocks.Math.Gain uPloadInputGain(k=1) annotation (Placement(
                  transformation(
                  extent={{-10,-10},{10,10}},
                  rotation=0,
                  origin={-152,-110})));
            Modelica.Blocks.Math.Gain uAVRInputGain(k=1/100) annotation (
                Placement(transformation(
                  extent={{-10,-10},{10,10}},
                  rotation=0,
                  origin={-152,114})));
            Interfaces.SMIB_AVR_wLineRmovaland4inputs
              sMIB_AVR_wLineRmovaland4inputs annotation (Placement(
                  transformation(extent={{-100,-138},{180,142}})));
            Modelica.Blocks.Interfaces.RealInput uVref
              annotation (Placement(transformation(extent={{-240,94},{-200,134}}),
                  iconTransformation(extent={{-180,60},{-140,100}})));
          equation
            connect(Pmchange.y,pmInputGain. u)
              annotation (Line(points={{-179,0},{-164,0}},   color={0,0,127}));
            connect(Ploadchange.y,uPloadInputGain. u)
              annotation (Line(points={{-179,-110},{-164,-110}},
                                                             color={0,0,127}));
            connect(uAVRInputGain.y, sMIB_AVR_wLineRmovaland4inputs.uvsAVR)
              annotation (Line(points={{-141,114},{-114,114}}, color={0,0,127}));
            connect(pmInputGain.y, sMIB_AVR_wLineRmovaland4inputs.uPm)
              annotation (Line(points={{-141,0},{-134,0},{-134,2},{-114,2}},
                  color={0,0,127}));
            connect(uPloadInputGain.y, sMIB_AVR_wLineRmovaland4inputs.uPload)
              annotation (Line(points={{-141,-110},{-114,-110}}, color={0,0,127}));
            connect(uVref, uAVRInputGain.u) annotation (Line(points={{-220,114},
                    {-164,114}}, color={0,0,127}));
            annotation (Icon(coordinateSystem(extent={{-200,-200},{200,200}})), Diagram(
                  coordinateSystem(extent={{-200,-200},{200,200}}), graphics={
                  Line(
                    points={{-124,178},{-194,180},{-152,130}},
                    color={0,0,255},
                    thickness=0.5,
                    arrow={Arrow.None,Arrow.Filled},
                    smooth=Smooth.Bezier),
                  Text(
                    extent={{-118,198},{-40,138}},
                    lineColor={85,170,255},
                    fillPattern=FillPattern.HorizontalCylinder,
                    fillColor={28,108,200},
                    horizontalAlignment=TextAlignment.Left,
                    textString="Note: we use this gain to scale the input to the AVR.
 This allows to apply e.g. a 1% step change when applying a unitary step into the input.
 This facilitates to use the existing routines of the Modelica.LinearSystems2 library.

If this is not done, then a unitary step signal can be too large and give unintuitive results.
")}));
          end NonlinearModelforDesign_AVR;
        end ModelVariants;

        function RootLocus_AVR

        algorithm

        Modelica_LinearSystems2.Utilities.Plot.rootLocusOfModel(
        "SMIBPS_IdControl.Analysis.ClassicalControlDesign.RootLocus.Design.ModelVariants.NonlinearModelforDesign_AVR",
        {Modelica_LinearSystems2.Records.ParameterVariation(Name="sMIB_AVR_wLineRmovaland4inputs.G1.avr.K0",
        grid=Modelica_LinearSystems2.Utilities.Types.Grid.Equidistant, Value=22, Min=0, Max=900, nPoints=100)});

        end RootLocus_AVR;
      end Design;

      package TimeDomainVerficationSimulations
      end TimeDomainVerficationSimulations;
    end RootLocus;
  end ClassicalControlDesign;
end Analysis;
