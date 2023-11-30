-- /input What's your favorite drink? |
-- /if left={{pipe}} right="black tea" rule=eq else="/echo You shall not pass \| /abort" "/echo Welcome to the club, \{\{user\}\}"

if input("What's your favorite drink?") == "black tea" then
    echo("Welcome to the club, {{user}}")
else
    echo("You shall not pass")
    break
end