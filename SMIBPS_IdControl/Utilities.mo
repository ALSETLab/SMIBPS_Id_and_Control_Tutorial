within SMIBPS_IdControl;
package Utilities
  extends Modelica.Icons.UtilitiesPackage;
  package Icons
    extends Modelica.Icons.IconsPackage;
    package FunctionDependentExample
      "f+m = for this model, a function drives the simulation of the model"
      annotation (Icon(graphics={
            Ellipse(
              lineColor={108,88,49},
              fillColor={213,255,170},
              fillPattern=FillPattern.Solid,
              extent={{-100,-100},{100,100}}),
            Polygon(lineColor={0,0,255},
                    fillColor={105,193,102},
                    pattern=LinePattern.None,
                    fillPattern=FillPattern.Solid,
                    points={{-28,66},{72,6},{-28,-54},{-28,66}}),
            Text(
              lineColor={0,140,72},
              extent={{-100,-60},{100,68}},
              textString="f+m")}), Documentation(info="<html>
<p><b><span style=\"font-size: 24pt;\">f+m Example</span></b></p>
<p>DO NOT try to run this model on it&apos;s own! </p>
<p>Models with this icon will not simulate on their own, instead they work together with a function that populates certain parameters in the model and perform other operations.</p>
<p>See the associated function to run.</p>
</html>"));
    end FunctionDependentExample;
  end Icons;
end Utilities;
