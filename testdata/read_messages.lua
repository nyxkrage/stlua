-- /setvar key=start {{lastMessageId}} |
-- /addvar key=start -2 |
-- /messages names=off {{getvar::start}}-{{lastMessageId}} |
-- /setinput

start = "{{lastMessageId}}"
start = start - 2
setinput(messages(false, "start-{{lastMessageId}}"))

