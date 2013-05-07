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
  connect(gaspowerplant1.u,const.y) annotation(Line(points = {{-48.9776,39.9645},{-66.874,39.9645},{-66.874,80.6011},{-66.533,80.6011}}));
  connect(solar1.pin_p,transformer1.pin_n) annotation(Line(points = {{15.6431,74.4645},{31.1042,74.4645},{31.1042,16.1742},{14.6645,16.1742},{14.6645,16.307}}));
  connect(transformer1.pin_n,industry1.pin_p) annotation(Line(points = {{14.6645,16.307},{30.7932,16.307},{30.7932,-18.3515},{54.3415,-18.3515},{54.3415,-18.6175}}));
  connect(transformer1.pin_n,household1.pin_p) annotation(Line(points = {{14.6645,16.307},{31.1042,16.307},{31.1042,37.3934},{54.3416,37.3934}}));
  connect(wind1.pin_p,transformer1.pin_p) annotation(Line(points = {{-34.9032,-53.1312},{-12.1306,-53.1312},{-12.1306,16.7963},{-4.26883,16.7963},{-4.26883,16.507}}));
  connect(storageplant1.pin_p,transformer1.pin_p) annotation(Line(points = {{-32.8574,-1.08353},{-12.1306,-1.08353},{-12.1306,16.7963},{-4.26883,16.7963},{-4.26883,16.507}}));
  connect(gaspowerplant1.pin_p,transformer1.pin_p) annotation(Line(points = {{-32.4443,41.1311},{-12.4417,41.1311},{-12.4417,16.7963},{-4.26883,16.7963},{-4.26883,16.507}}));
  connect(coalpowerplant1.pin_p,transformer1.pin_p) annotation(Line(points = {{-31.6246,81.3279},{-12.4417,81.3279},{-12.4417,16.7963},{-4.26883,16.7963},{-4.26883,16.507}}));
  connect(const.y,coalpowerplant1.u) annotation(Line(points = {{-66.533,80.6011},{-48.2115,80.6011},{-48.2115,80.1285},{-48.1579,80.1285}}));
end powerNetwork;

