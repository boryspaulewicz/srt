## -*- coding: utf-8 -*-

MAX.REACTION.TIME = 3000
FIXATION.TIME = 1000
POST.FIXATION.MIN = 1000

## Globalne obiekty graficzne

RECT = center(new(RectangleShape, WINDOW$get.size()), WINDOW)
RECT$set.position(WINDOW$get.size() / 2)
TXT$set.string("Proszę nacisnąć spację")
center(TXT, WINDOW)
FX = fixation(WINDOW)

## Tylko jeden klawisz w kluczu reakcyjnym

KEYS <<- CORRECT.KEY <<- Key.Space

trial.code = function(trial, size = .1, intensity = .5){
    ## Kod specyficzny dla zadania
    rsoa = runif(1) * 1000
    RECT$set.fill.color(rep(intensity, 3))
    RECT$set.scale(rep(size, 2))
    ## Szablon
    if(trial == 1){
        state = 'press-space'
    }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > (POST.FIXATION.MIN + rsoa)){
                state = 'show-stim'
            }
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(RECT)
            WINDOW$display()
            stim.onset = CLOCK$time
            ACC <<- RT <<- NULL
            state = 'measure-reaction'
        }, 'measure-reaction' = {
            if(!is.null(ACC) || ((CLOCK$time - stim.onset) > MAX.REACTION.TIME))state = 'done'
        }, 'done' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            return(list(soa = floor(rsoa), rt = ifelse(is.null(RT), MAX.REACTION.TIME, RT - stim.onset),
                        acc = ifelse(is.null(ACC), 2, ACC)))
        })
    }
}

## gui.user.data()
USER.DATA = list(name = 'admin', age = 37, gender = 'M')
run.trials(trial.code, expand.grid(size = c(.1, .2), intensity = c(.2, .6, 1)))
if(!interactive())quit("no")
