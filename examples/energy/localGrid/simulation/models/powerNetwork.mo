model powerNetwork
  GHSimulation.Grid.HouseHold household1 annotation(Placement(visible = true, transformation(origin = {62.8416,33.0601}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.Industry industry1 annotation(Placement(visible = true, transformation(origin = {62.8415,-22.9508}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.Solar solar1 annotation(Placement(visible = true, transformation(origin = {8.74312,72.1312}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.Transformer transformer1 annotation(Placement(visible = true, transformation(origin = {5.4645,15.5737}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.CoalPowerPlant coalpowerplant1 annotation(Placement(visible = true, transformation(origin = {-38.5246,78.9618}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.StoragePlant storageplant1 annotation(Placement(visible = true, transformation(origin = {-39.8907,-7.65019}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.GasPowerPlant gaspowerplant1 annotation(Placement(visible = true, transformation(origin = {-39.3443,38.7978}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.Wind wind1 annotation(Placement(visible = true, transformation(origin = {-41.8032,-55.4645}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const annotation(Placement(visible = true, transformation(origin = {-72.1311,80.6011}, extent = {{-5.08917,-5.08917},{5.08917,5.08917}}, rotation = 0)));
equation
  connect(const.y,coalpowerplant1.u) annotation(Line(points = {{-66.5331,80.6011},{-48.6339,80.6011},{-48.6339,80.1284},{-48.158,80.1284}}));
  connect(gaspowerplant1.pin_p,transformer1.pin_p) annotation(Line(points = {{-32.4443,41.1311},{-13.9344,41.1311},{-13.9344,17.4863},{-4.26884,17.4863},{-4.26884,16.5071}}));
  connect(coalpowerplant1.pin_p,transformer1.pin_p) annotation(Line(points = {{-31.6246,81.2951},{-13.9344,81.2951},{-13.9344,16.9399},{-9.83607,16.9399},{-9.83607,16.5071},{-4.26884,16.5071}}));
  connect(storageplant1.pin_p,transformer1.pin_p) annotation(Line(points = {{-32.8574,-1.08352},{-13.9344,-1.08352},{-13.9344,16.9399},{-4.26884,16.9399},{-4.26884,16.5071}}));
  connect(wind1.pin_p,transformer1.pin_p) annotation(Line(points = {{-34.9032,-53.1312},{-13.9344,-53.1312},{-13.9344,17.2131},{-4.26884,17.2131},{-4.26884,16.5071}}));
  connect(solar1.pin_p,transformer1.pin_n) annotation(Line(points = {{15.6431,74.4645},{22.4044,74.4645},{22.4044,16.6667},{14.6645,16.6667},{14.6645,16.3071}}));
  connect(transformer1.pin_n,industry1.pin_p) annotation(Line(points = {{14.6645,16.3071},{22.1311,16.3071},{22.1311,-18.8525},{54.3415,-18.8525},{54.3415,-18.6175}}));
  connect(transformer1.pin_n,household1.pin_p) annotation(Line(points = {{14.6645,16.3071},{22.4044,16.3071},{22.4044,37.4317},{54.3416,37.4317},{54.3416,37.3934}}));
end powerNetwork;

