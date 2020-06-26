package SMIBPS_IdControl "A Tutorial on Power System Stability and Control"





annotation(preferredView = "info",
  uses(                           Modelica(version="3.2.3"),
      Modelica_LinearSystems2(version="2.3.5"),
      DataFiles(version="1.0.5"),
    OpenIPSL(version="1.5.0"),
    DymolaCommands(version="1.9")),
  version="1",
  Documentation(info="<html>
<p><br>This package contains examples on using Modelica, OpenIPSL and the Modelica Linear Systems 2 library to carry out typical power system stability and control studies.</p>
<p>The goal is to illustrate how models need to be defined for linearization when different model variants need to be compared.</p>
<p>Another goal is to illustrate the use of the MLS2 library for typical power system control design tasks, specifically the so-called Power System Stabilizer.</p>
<p><br>The analysis examples are developed using the single-machine infinite bus model used in the Example 13.2 of the Kundur&apos;s book as originally implemented in PSAT, and latter on ported to OpenIPSL.</p>
<p><br>(c) 2020-2050, Luigi Vanfretti, Rensselaer Polytechnic Institute, Troy, NY, USA.</p>
</html>"),
    Icon(graphics={
        Rectangle(
          lineColor={85,170,255},
          fillColor={28,108,200},
          fillPattern=FillPattern.HorizontalCylinder,
          extent={{-100,-98},{100,102}},
          radius=25),
        Rectangle(
          lineColor={128,128,128},
          extent={{-100,-98},{100,102}},
          radius=25.0),
      Rectangle(
        origin={0,35.149},
        fillColor={255,255,255},
        extent={{-30.0,-20.1488},{30.0,20.1488}},
          lineColor={255,255,255},
          lineThickness=1),
      Rectangle(
        origin={0,-34.851},
        fillColor={255,255,255},
        extent={{-30.0,-20.1488},{30.0,20.1488}},
          lineColor={255,255,255},
          lineThickness=1),
      Line(
        origin={-51.25,0},
        points={{21.25,-35.0},{-13.75,-35.0},{-13.75,35.0},{6.25,35.0}},
          color={255,255,255},
          thickness=1),
      Polygon(
        origin={-40,35},
        pattern=LinePattern.None,
        points={{10.0,0.0},{-5.0,5.0},{-5.0,-5.0}},
          lineColor={255,255,255},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid),
      Line(
        origin={51.25,0},
        points={{-21.25,35.0},{13.75,35.0},{13.75,-35.0},{-6.25,-35.0}},
          color={255,255,255},
          thickness=1),
      Polygon(
        origin={40,-35},
        pattern=LinePattern.None,
        points={{-10.0,0.0},{5.0,5.0},{5.0,-5.0}},
          lineColor={255,255,255},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid)}));
end SMIBPS_IdControl;
