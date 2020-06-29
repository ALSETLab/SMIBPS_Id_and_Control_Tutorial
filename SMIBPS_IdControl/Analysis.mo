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
          annotation (Placement(transformation(extent={{-116,-12},{-92,12}})));
        Modelica.Blocks.Interfaces.RealInput uVref
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
        connect(uVref, G1.uVs) annotation (Line(points={{-160,40},{-134,40},{
                -134,7.2},{-118.64,7.2}},
                                color={0,0,127}));
        connect(G1.pm, uPm) annotation (Line(points={{-118.4,-7.2},{-134,-7.2},
                {-134,-40},{-160,-40}},
                             color={0,0,127}));
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-90.8,0},{-80,0}}, color={0,0,255}));
        connect(uPload, load_ExtInput.u) annotation (Line(points={{-160,-100},{
                -72,-100},{-72,-66.7},{17.14,-66.7}}, color={0,0,127}));
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
        connect(G1.uPSS, uPSS) annotation (Line(points={{-122,6},{-128,6},{-128,
                40},{-160,40}},
                      color={0,0,127}));
        connect(uPm, G1.pm) annotation (Line(points={{-160,-40},{-130,-40},{
                -130,-6},{-122,-6}},
                           color={0,0,127}));
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-99,0},{-80,0}}, color={0,0,255}));
        connect(load_ExtInput.u, uPload) annotation (Line(points={{17.14,-66.7},
                {-18,-66.7},{-18,-100},{-160,-100}}, color={0,0,127}));
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

      model SMIB_AVR_PSS_wInput_wFault
        extends BaseModelsPartial.BaseNetwork.SMIB_Partial(powerFlow_Data(
            redeclare record Bus = PF_Data.Bus_Data.PF_Bus_5,
            redeclare record Loads = PF_Data.Loads_Data.PF_Loads_5,
            redeclare record Trafos = PF_Data.Trafos_Data.PF_Trafos_5,
            redeclare record Machines = PF_Data.Machines_Data.PF_Machines_5),
          fault(
            R=R,
            X=X,
            t1=t1,
            t2=t2),
          line_3(
            t1=t1_line,
            t2=t2_line,
            opening=opening_line),
          line_2(t1=Modelica.Constants.inf));
        extends SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        import Modelica.Constants.pi;
        BaseModelsPartial.BasePlants.Generator_AVR_PSS_wInputs G1(
          V_0=powerFlow_Data.bus.V1,
          P_0=powerFlow_Data.machines.PG1,
          Q_0=powerFlow_Data.machines.QG1,
          angle_0=powerFlow_Data.bus.A1)
          annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
        Modelica.Blocks.Interfaces.RealInput uPSS
          annotation (Placement(transformation(extent={{-184,80},{-144,120}})));
        Modelica.Blocks.Interfaces.RealInput uPm
          annotation (Placement(transformation(extent={{-180,-20},{-140,20}})));
        Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
              transformation(extent={{-180,-120},{-140,-80}})));
        parameter Real R=0 "Resistance (pu)"
          annotation (Dialog(group="Fault Parameters"));
        parameter Real X=1e-5 "Reactance (pu)"
          annotation (Dialog(group="Fault Parameters"));
        parameter Real t1=0.5 "Start time of the fault (s)"
          annotation (Dialog(group="Fault Parameters"));
        parameter Real t2=0.57 "End time of the fault (s)"
          annotation (Dialog(group="Fault Parameters"));
        parameter Modelica.SIunits.Time t1_line=0.57
          annotation (Dialog(group="Line Opening Parameters"));
        parameter Modelica.SIunits.Time t2_line=Modelica.Constants.inf
          annotation (Dialog(group="Line Opening Parameters"));
        parameter Integer opening_line=1
          annotation (Dialog(group="Line Opening Parameters"));
      protected
        parameter Real S_b=SysData.S_b;
      equation
        w = G1.machine.w;
        delta = G1.machine.delta;
        Vt = G1.machine.v;
        P = G1.machine.P;
        Q = G1.machine.Q;
        connect(load_ExtInput.u, uPload) annotation (Line(points={{17.14,-66.7},{-2,-66.7},
                {-2,-100},{-160,-100}},              color={0,0,127}));
        connect(G1.uPSS, uPSS) annotation (Line(points={{-122,6},{-128,6},{-128,
                100},{-164,100}},
                      color={0,0,127}));
        connect(uPm, G1.pm) annotation (Line(points={{-160,0},{-130,0},{-130,-6},
                {-122,-6}},color={0,0,127}));
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-99,0},{-80,0}}, color={0,0,255}));
        annotation (
          Diagram(coordinateSystem(extent={{-140,-140},{140,140}})),
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
      end SMIB_AVR_PSS_wInput_wFault;

      partial model OutputsInterface
        Modelica.Blocks.Interfaces.RealOutput Vt
          annotation (Placement(transformation(extent={{140,68},{160,90}})));
      public
        Modelica.Blocks.Interfaces.RealOutput Q
          annotation (Placement(transformation(extent={{140,-10},{160,12}})));
        Modelica.Blocks.Interfaces.RealOutput P
          annotation (Placement(transformation(extent={{140,30},{160,52}})));
        Modelica.Blocks.Interfaces.RealOutput w
          annotation (Placement(transformation(extent={{140,-50},{160,-28}})));
        Modelica.Blocks.Interfaces.RealOutput delta
          annotation (Placement(transformation(extent={{140,-92},{160,-70}})));
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
      extends Modelica.Icons.ExamplesPackage;
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
        connect(Vt, PS_wAVR.Vt) annotation (Line(points={{150,79},{98,79},{98,
                22.5714},{42.8571,22.5714}},
                                    color={0,0,127}));
        connect(P, PS_wAVR.P) annotation (Line(points={{150,41},{112,41},{112,
                11.7143},{42.8571,11.7143}},
                                    color={0,0,127}));
        connect(Q, PS_wAVR.Q) annotation (Line(points={{150,1},{96,1},{96,
                0.285714},{42.8571,0.285714}},
                            color={0,0,127}));
        connect(w, PS_wAVR.w) annotation (Line(points={{150,-39},{108,-39},{108,
                -11.1429},{42.8571,-11.1429}},
                                     color={0,0,127}));
        connect(delta, PS_wAVR.delta) annotation (Line(points={{150,-81},{92,
                -81},{92,-23.1429},{42.8571,-23.1429}},
                                               color={0,0,127}));
        connect(vsInputGain.u, stepVs.y)
          annotation (Line(points={{-82,20},{-99,20}}, color={0,0,127}));
        connect(vsInputGain.y, PS_wAVR.uVref) annotation (Line(points={{-59,20},
                {-54,20},{-54,11.4286},{-45.7143,11.4286}},
                                                   color={0,0,127}));
        connect(pmInputGain.y, PS_wAVR.uPm) annotation (Line(points={{-59,-20},
                {-54,-20},{-54,-11.4286},{-45.7143,-11.4286}},
                                                     color={0,0,127}));
        connect(Pmchange.y, pmInputGain.u)
          annotation (Line(points={{-99,-20},{-82,-20}}, color={0,0,127}));
        connect(Ploadchange.y, uPloadInputGain.u)
          annotation (Line(points={{-101,-60},{-86,-60}}, color={0,0,127}));
        connect(uPloadInputGain.y, PS_wAVR.uPload) annotation (Line(points={{-63,-60},
                {-56,-60},{-56,-28.5714},{-45.7143,-28.5714}},          color={
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
      extends Modelica.Icons.ExamplesPackage;
      model NonlinModel_for_Simulation
        "Model that includes a line removal at 5 seconds for linearization at initialization and after the line removal"
        extends Modelica.Icons.Example;
        Modelica.Blocks.Interfaces.RealOutput Vt
          annotation (Placement(transformation(extent={{118,68},{138,90}})));
      public
        Modelica.Blocks.Interfaces.RealOutput Q
          annotation (Placement(transformation(extent={{118,-10},{138,12}})));
        Modelica.Blocks.Interfaces.RealOutput P
          annotation (Placement(transformation(extent={{118,30},{138,52}})));
        Modelica.Blocks.Interfaces.RealOutput w
          annotation (Placement(transformation(extent={{118,-50},{138,-28}})));
        Modelica.Blocks.Interfaces.RealOutput delta
          annotation (Placement(transformation(extent={{118,-92},{138,-70}})));
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
        Interfaces.SMIB_AVR_PSS_wInput_wFault
          sMIB_AVR_PSS_wInput_wFault(
          R=0,
          X=1e-3,
          t1=Modelica.Constants.inf,
          t2=Modelica.Constants.inf,
          t1_line=0.5,
          t2_line=Modelica.Constants.inf,
          opening_line=1)
          annotation (Placement(transformation(extent={{-20,-20},{36,36}})));
      equation
        connect(efdInputGain.u,constEfd. y)
          annotation (Line(points={{-104,40},{-121,40}},
                                                       color={0,0,127}));
        connect(Pmchange.y,pmInputGain. u)
          annotation (Line(points={{-119,0},{-104,0}},   color={0,0,127}));
        connect(Ploadchange.y,uPloadInputGain. u)
          annotation (Line(points={{-121,-40},{-106,-40}},
                                                         color={0,0,127}));
        connect(efdInputGain.y,sMIB_AVR_PSS_wInput_wFault. uPSS) annotation (Line(
              points={{-81,40},{-34,40},{-34,28},{-24.8,28}},
                                                         color={0,0,127}));
        connect(pmInputGain.y,sMIB_AVR_PSS_wInput_wFault. uPm)
          annotation (Line(points={{-81,0},{-52,0},{-52,8},{-24,8}},
                                                    color={0,0,127}));
        connect(uPloadInputGain.y,sMIB_AVR_PSS_wInput_wFault. uPload) annotation (
            Line(points={{-83,-40},{-36,-40},{-36,-12},{-24,-12}},
                                                                color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wFault.Vt,Vt)  annotation (Line(points={{38,23.8},
                {64.5,23.8},{64.5,79},{128,79}},       color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wFault.P,P)  annotation (Line(points={{38,16.2},
                {94,16.2},{94,41},{128,41}},       color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wFault.w,w)  annotation (Line(points={{38,0.2},
                {63.5,0.2},{63.5,-39},{128,-39}},      color={0,0,127}));
        connect(sMIB_AVR_PSS_wInput_wFault.delta,delta)  annotation (Line(
              points={{38,-8.2},{38.5,-8.2},{38.5,-81},{128,-81}}, color={0,0,
                127}));
        connect(sMIB_AVR_PSS_wInput_wFault.Q, Q) annotation (Line(points={{38,
                8.2},{81,8.2},{81,1},{128,1}}, color={0,0,127}));
        annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{
                  -160,-100},{120,100}})),                             Diagram(
              coordinateSystem(preserveAspectRatio=false, extent={{-160,-100},{
                  120,100}})),
          experiment(
            StopTime=20,
            __Dymola_NumberOfIntervals=5000,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.001,
            __Dymola_Algorithm="Dassl"));
      end NonlinModel_for_Simulation;

      package LinAtZero
        extends Modelica.Icons.ExamplesPackage;
        model NonlinModel_for_Linearization
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
          Interfaces.SMIB_AVR_PSS_wInput_wFault
            sMIB_AVR_PSS_wInput_wFault(
            R=0,
            X=1e-3,
            t1=Modelica.Constants.inf,
            t2=Modelica.Constants.inf,
            t1_line=0.5,
            t2_line=Modelica.Constants.inf,
            opening_line=1)
            annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
          Modelica.Blocks.Interfaces.RealInput uPSS
            annotation (Placement(transformation(extent={{-140,40},{-100,80}})));
          Modelica.Blocks.Interfaces.RealInput uPm
            annotation (Placement(transformation(extent={{-140,-20},{-100,20}})));
          Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
                transformation(extent={{-140,-80},{-100,-40}})));
        equation
          connect(uPSS, sMIB_AVR_PSS_wInput_wFault.uPSS) annotation (Line(points={{-120,60},
                  {-84,60},{-84,28.5714},{-46.8571,28.5714}},     color={0,0,127}));
          connect(uPm, sMIB_AVR_PSS_wInput_wFault.uPm)
            annotation (Line(points={{-120,0},{-45.7143,0}}, color={0,0,127}));
          connect(uPload, sMIB_AVR_PSS_wInput_wFault.uPload) annotation (Line(points={{-120,
                  -60},{-80,-60},{-80,-28.5714},{-45.7143,-28.5714}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.Vt, Vt) annotation (Line(points={{42.8571,
                  22.5714},{68,22.5714},{68,79},{108,79}},
                                                  color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.P, P) annotation (Line(points={{42.8571,
                  11.7143},{82,11.7143},{82,41},{108,41}},
                                                  color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.Q, Q) annotation (Line(points={{42.8571,
                  0.285714},{72.4286,0.285714},{72.4286,1},{108,1}},
                                                           color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.w, w) annotation (Line(points={{42.8571,
                  -11.1429},{84,-11.1429},{84,-39},{108,-39}},
                                                     color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.delta, delta) annotation (Line(points={{42.8571,
                  -23.1429},{60,-23.1429},{60,-81},{108,-81}}, color={0,0,127}));
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
          Interfaces.SMIB_AVR_PSS_wInput_wFault
            sMIB_AVR_PSS_wInput_wFault(
            R=0,
            X=1e-3,
            t1=Modelica.Constants.inf,
            t2=Modelica.Constants.inf,
            t1_line=Modelica.Constants.inf,
            t2_line=Modelica.Constants.inf,
            opening_line=1)
            annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-100,20},{-80,40}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-100,-10},{-80,10}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.1,
            offset=0,
            startTime=1)                                    annotation (Placement(
                transformation(extent={{-100,-40},{-80,-20}})));
        equation
          connect(sMIB_AVR_PSS_wInput_wFault.Vt, Vt) annotation (Line(points={{42.8571,
                  22.5714},{68,22.5714},{68,79},{108,79}},
                                                  color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.P, P) annotation (Line(points={{42.8571,
                  11.7143},{82,11.7143},{82,41},{108,41}},
                                                  color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.Q, Q) annotation (Line(points={{42.8571,
                  0.285714},{72.4286,0.285714},{72.4286,1},{108,1}},
                                                           color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.w, w) annotation (Line(points={{42.8571,
                  -11.1429},{84,-11.1429},{84,-39},{108,-39}},
                                                     color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.delta, delta) annotation (Line(points={{42.8571,
                  -23.1429},{60,-23.1429},{60,-81},{108,-81}}, color={0,0,127}));
          connect(PSSchange.y, sMIB_AVR_PSS_wInput_wFault.uPSS) annotation (Line(
                points={{-79,30},{-60,30},{-60,28.5714},{-46.8571,28.5714}},
                color={0,0,127}));
          connect(Pmchange.y, sMIB_AVR_PSS_wInput_wFault.uPm)
            annotation (Line(points={{-79,0},{-45.7143,0}}, color={0,0,127}));
          connect(Ploadchange.y, sMIB_AVR_PSS_wInput_wFault.uPload) annotation (
             Line(points={{-79,-30},{-64,-30},{-64,-28.5714},{-45.7143,-28.5714}},
                color={0,0,127}));
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
          input Modelica.SIunits.Time tlin = 0 "t for model linearization";
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
          Interfaces.SMIB_AVR_PSS_wInput_wFault
            sMIB_AVR_PSS_wInput_wFault(
            R=0,
            X=1e-3,
            t1=Modelica.Constants.inf,
            t2=Modelica.Constants.inf,
            t1_line=0.5,
            t2_line=Modelica.Constants.inf,
            opening_line=1)
            annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
          Modelica.Blocks.Interfaces.RealInput uPSS
            annotation (Placement(transformation(extent={{-140,40},{-100,80}})));
          Modelica.Blocks.Interfaces.RealInput uPm
            annotation (Placement(transformation(extent={{-140,-20},{-100,20}})));
          Modelica.Blocks.Interfaces.RealInput uPload annotation (Placement(
                transformation(extent={{-140,-80},{-100,-40}})));
        equation
          connect(uPSS, sMIB_AVR_PSS_wInput_wFault.uPSS) annotation (Line(points={{-120,60},
                  {-84,60},{-84,28.5714},{-46.8571,28.5714}},     color={0,0,127}));
          connect(uPm, sMIB_AVR_PSS_wInput_wFault.uPm)
            annotation (Line(points={{-120,0},{-45.7143,0}}, color={0,0,127}));
          connect(uPload, sMIB_AVR_PSS_wInput_wFault.uPload) annotation (Line(points={{-120,
                  -60},{-80,-60},{-80,-28.5714},{-45.7143,-28.5714}}, color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.Vt, Vt) annotation (Line(points={{42.8571,
                  22.5714},{68,22.5714},{68,79},{108,79}},
                                                  color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.P, P) annotation (Line(points={{42.8571,
                  11.7143},{82,11.7143},{82,41},{108,41}},
                                                  color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.Q, Q) annotation (Line(points={{42.8571,
                  0.285714},{72.4286,0.285714},{72.4286,1},{108,1}},
                                                           color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.w, w) annotation (Line(points={{42.8571,
                  -11.1429},{84,-11.1429},{84,-39},{108,-39}},
                                                     color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.delta, delta) annotation (Line(points={{42.8571,
                  -23.1429},{60,-23.1429},{60,-81},{108,-81}}, color={0,0,127}));
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
          Interfaces.SMIB_AVR_PSS_wInput_wFault
            sMIB_AVR_PSS_wInput_wFault(
            R=0,
            X=1e-3,
            t1=Modelica.Constants.inf,
            t2=Modelica.Constants.inf,
            t1_line=0.5,
            t2_line=Modelica.Constants.inf,
            opening_line=1)
            annotation (Placement(transformation(extent={{-40,-42},{40,38}})));
          Modelica.Blocks.Sources.Constant PSSchange(k=0)
            annotation (Placement(transformation(extent={{-100,20},{-80,40}})));
          Modelica.Blocks.Sources.Constant Pmchange(k=0)
            annotation (Placement(transformation(extent={{-100,-10},{-80,10}})));
          Modelica.Blocks.Sources.Step     Ploadchange(
            height=0.05,
            offset=0,
            startTime=20.5)                                 annotation (Placement(
                transformation(extent={{-100,-42},{-80,-22}})));
        equation
          connect(sMIB_AVR_PSS_wInput_wFault.Vt, Vt) annotation (Line(points={{42.8571,
                  20.5714},{68,20.5714},{68,79},{108,79}},
                                                  color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.P, P) annotation (Line(points={{42.8571,
                  9.71429},{82,9.71429},{82,41},{108,41}},
                                                  color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.Q, Q) annotation (Line(points={{42.8571,
                  -1.71429},{72.4286,-1.71429},{72.4286,1},{108,1}},
                                                           color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.w, w) annotation (Line(points={{42.8571,
                  -13.1429},{84,-13.1429},{84,-39},{108,-39}},
                                                     color={0,0,127}));
          connect(sMIB_AVR_PSS_wInput_wFault.delta, delta) annotation (Line(points={{42.8571,
                  -25.1429},{60,-25.1429},{60,-81},{108,-81}}, color={0,0,127}));
          connect(PSSchange.y, sMIB_AVR_PSS_wInput_wFault.uPSS) annotation (Line(
                points={{-79,30},{-60,30},{-60,26.5714},{-46.8571,26.5714}},
                color={0,0,127}));
          connect(Pmchange.y, sMIB_AVR_PSS_wInput_wFault.uPm)
            annotation (Line(points={{-79,0},{-62,0},{-62,-2},{-45.7143,-2}},
                                                            color={0,0,127}));
          connect(Ploadchange.y, sMIB_AVR_PSS_wInput_wFault.uPload) annotation (
             Line(points={{-79,-32},{-64,-32},{-64,-30.5714},{-45.7143,-30.5714}},
                color={0,0,127}));
          annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
                coordinateSystem(preserveAspectRatio=false)));
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
              A=[0.0,376.9911184608106,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0;
                  -0.12324605225841194,0.0,0.0,0.0,-0.163440553796113,
                  0.02162438701783512,0.0,0.0,0.0,0.0,0.0,0.0; -0.21219255041804597,
                  0.0,-0.1249999999580813,0.0,-0.21288042148990358,-0.0013680675599648574,
                  0.0,0.0,0.1250000000076102,0.0,0.0,0.0; 0.17623055679033187,
                  0.0,0.0,-1.0000000000353078,0.012188165598172153,-1.205477672669465,
                  0.0,0.0,0.0,0.0,0.0,0.0; -2.7938441236330394,0.0,
                  33.33333332999551,0.0,-36.136234348612966,-0.018012731997358068,
                  0.0,0.0,0.0,0.0,0.0,0.0; 1.0019935462686163,0.0,0.0,
                  14.285714286218683,0.06929821622411289,-21.139695059192498,
                  0.0,0.0,0.0,0.0,0.0,0.0; -10.044241452053852,0.0,0.0,0.0,
                  33.749197392269075,35.10053247290774,-66.66666667139577,0.0,
                  0.0,0.0,0.0,0.0; 0.0,0.0,0.0,0.0,0.0,0.0,0.0,-1.0,0.0,0.0,0.0,
                  0.0; 0.0,19000000.001291934,0.0,0.0,0.0,0.0,-2000000.000141873,
                  10000.00082740371,-10000.000000608816,0.0,0.0,-18999999.99984346;
                  0.0,0.009500000000645967,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.0010000000000000007,
                  0.0,-0.009499999999921729; 0.0,0.009500000000645967,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0,0.0,-0.0010000000000000007,-0.009499999999921729;
                  0.0,0.7092198582125331,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.7092198581708853],
              B=[0.0,0.0,0.0; 0.0,0.14285715467719584,-0.08487555103187104; 0.0,
                  0.0,-0.09261674760452365; 0.0,0.0,0.33888530870385125; 0.0,
                  0.0,-1.2194430650443628; 0.0,0.0,1.9267991963849742; 0.0,0.0,
                  -1.4717116414431057; 0.0,0.0,0.0; 19000001.57206705,0.0,0.0;
                  0.009500000786033524,0.0,0.0; 0.009500000786033524,0.0,0.0;
                  0.709219838097921,0.0,0.0],
              C=[-0.15066362178080778,0.0,0.0,0.0,0.506237960884036,
                  0.526507987093616,0.0,0.0,0.0,0.0,0.0,0.0;
                  0.41780799296094867,0.0,0.0,0.0,1.3915831199685333,
                  0.5198880323747255,0.0,0.0,0.0,0.0,0.0,0.0;
                  0.8565983041014822,0.0,0.0,0.0,1.1383197063841153,-0.1486266791310306,
                  0.0,0.0,0.0,0.0,0.0,0.0; 0.0,1.0000000000796714,0.0,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0,0.0,0.0; 0.9999999999553499,0.0,0.0,0.0,
                  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
              D=[0.0,0.0,-0.022075674621646613; 0.0,0.0,-0.020085100249644938;
                  0.0,0.0,0.5908514788544039; 0.0,0.0,0.0; 0.0,0.0,0.0],
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
            startTime=20.5)                                 annotation (Placement(
                transformation(extent={{-124,-42},{-104,-22}})));
          Modelica.Blocks.Sources.Constant y0_initial[5](k={0.99954628944397,
                0.276092916727066,0.899889945983887,1,1.42118239402771})
            annotation (Placement(transformation(
                extent={{-10,-10},{10,10}},
                rotation=90,
                origin={0,-52})));
        equation
          connect(multiplex3_1.u1[1], const.y) annotation (Line(points={{-82,7},{
                  -90,7},{-90,32},{-103,32}}, color={0,0,127}));
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
          connect(demultiplex2_2.y3[1], P) annotation (Line(points={{102,0},{
                  122,0},{122,41},{150,41}}, color={0,0,127}));
          connect(Q, demultiplex2_2.y2[1]) annotation (Line(points={{150,1},{
                  126,1},{126,8},{102,8}}, color={0,0,127}));
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
          input Modelica.SIunits.Time tlin = 20 "t for model linearization";
          input Modelica.SIunits.Time tsim = 20 "Simulation time";
          input String pathToNonlinearPlantModel = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAfterDisturbance.NonlinModel_for_Linearization";
          input String pathToNonlinearExperiment = "SMIBPS_IdControl.Analysis.LinearAnalysis.LinearizeAfterDisturbance.LinAfterDisturbance.NonlinModel_for_NonlinExperiment";

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
           ylen := DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat");
           DataFiles.writeMATmatrix(
             "MyData.mat",
             "y0",
             [y0[1:ny,ylen]],
             append=true); // compared to the previous case, have used ylen, which is the number of points in the trajectory file
          Modelica.Utilities.Streams.print(String(ylen) + " number of points in the trajectory file");
          // Print y0's first values which is needed for the linear response model
          y0out := y0[:,ylen]; // here we want the values at the end
          Modelica.Utilities.Streams.print("y0 at selected time =");
          Modelica.Math.Vectors.toString(y0out);
          annotation(__Dymola_interactive=true);
        end LinearizeSimple;

        function Compare "Compares the responses"
          // INPUTS TO THE FUNCTION
          // Declare reconfigurable simulation parameters
          input Modelica.SIunits.Time tsim = 30 "Simulation time";
          input Integer numberOfIntervalsin=10000 "No. of intervals";
          input String methodin = "Rkfix4" "Solver";
          //input String methodin = "DASSL" "Solver";
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

        removePlots(false);
        createPlot(id=1, position={0, 0, 1311, 963}, y={"Vt"}, range={0.0, 30.0, 0.985, 1.01}, grid=true, filename="res_nl.mat", colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={0, 0, 1311, 963}, y={"Q"}, range={0.0, 30.0, 0.15000000000000002, 0.4}, grid=true, subPlot=102, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={0, 0, 1311, 963}, y={"P"}, range={0.0, 30.0, 0.7000000000000001, 1.1}, grid=true, subPlot=103, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={0, 0, 1311, 963}, y={"w"}, range={0.0, 30.0, 0.97, 1.01}, grid=true, subPlot=104, colors={{28,108,200}}, displayUnits={"1"});
        createPlot(id=1, position={0, 0, 1311, 963}, y={"delta"}, range={0.0, 30.0, 0.8, 1.6}, grid=true, subPlot=105, colors={{28,108,200}}, displayUnits={"rad"});
        createPlot(id=1, position={0, 0, 1311, 963}, y={"Vt"}, range={0.0, 30.0, 0.985, 1.01}, erase=false, grid=true, filename="res_lin.mat", colors={{238,46,47}});
        createPlot(id=1, position={0, 0, 1311, 963}, y={"Q"}, range={0.0, 30.0, 0.15000000000000002, 0.4}, erase=false, grid=true, subPlot=102, colors={{238,46,47}});
        createPlot(id=1, position={0, 0, 1311, 963}, y={"P"}, range={0.0, 30.0, 0.7000000000000001, 1.1}, erase=false, grid=true, subPlot=103, colors={{238,46,47}});
        createPlot(id=1, position={0, 0, 1311, 963}, y={"w"}, range={0.0, 30.0, 0.97, 1.01}, erase=false, grid=true, subPlot=104, colors={{238,46,47}});
        createPlot(id=1, position={0, 0, 1311, 963}, y={"delta"}, range={0.0, 30.0, 0.8, 1.6}, erase=false, grid=true, subPlot=105, colors={{238,46,47}});



          annotation(__Dymola_interactive=true, Documentation(info="<html>
<p>This function linearizes the model at two different times, initialization and at a user provided time.</p>
</html>"));
        end Compare;
      end LinAfterDisturbance;
    end LinearizeAfterDisturbance;
  end LinearAnalysis;

end Analysis;
