-- /setglobalvar key=PI 3.1415 |
-- /setvar key=r 50 |
-- /mul r r PI |
-- /round |
-- /echo Circle area: {{pipe}}

PI = 3.1415
r = 50
echo("Circle area: " .. round(r * r * PI))