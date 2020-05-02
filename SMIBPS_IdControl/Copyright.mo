within SMIBPS_IdControl;
class Copyright "Copyright"

  annotation (
    preferredView="info",
    DocumentationClass=false,
    Icon(graphics={
        Text(
          extent={{-100,140},{100,100}},
          lineColor={0,127,0},
          textString="%name%"),
        Ellipse(
          extent={{-100,100},{100,-100}},
          lineColor={0,127,0},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid),
        Ellipse(
          extent={{-60,60},{60,-60}},
          lineColor={0,127,72},
          fillColor={0,127,0},
          fillPattern=FillPattern.Solid),
        Ellipse(
          extent={{-40,40},{40,-40}},
          lineColor={255,255,255},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{28,22},{64,-20}},
          lineColor={255,255,255},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid)}),
    Documentation(info="<html>
<p><span style=\"font-family: Courier New;\">(c) 2020 - 2050. All rights reserved.</span></p>
<p><span style=\"font-family: Courier New;\">Luigi Vanfretti, Rensselaer Polytechnic Institute, Troy, NY.</span></p>
</html>"));

end Copyright;
