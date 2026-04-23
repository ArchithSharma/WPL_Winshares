# Cracking the WPL Code: Win shares and auction metrics in the Women's Premier League

## To run these files and replicate the results, run in the order: read_data.R, win_shares.R, plot_WS.R, and lastly optimization.R. No random seed is used.

The general inspiration of the project came from my earlier work on T20 Internationals presented at [CMSAC](https://www.linkedin.com/posts/archithsharma_cmsac-analytics-cricket-ugcPost-7393056316971315200-6Kgj/) [https://archithsharma.github.io/CricketPredictions/](https://archithsharma.github.io/CricketPredictions/),
and the recent success of the Indian Women's Cricket Team in the 2025 ICC Women's World Cup, which included a nomination for the Laureus World Sports Awards for Team of the Year. 
I wanted to apply a more valid approach using ball-by-ball data to evaluate player performance that standardizing for phases, and to see how the auction metrics of the WPL relate to player performance. 
I also wanted to see if there were any insights that could be drawn from the data that could be used to optimize team composition for the next season of the WPL.

The data folder contains data files from Cricsheet in JSON format. The data has 22 matches (from Cricsheet) and auction data sourced from ESPNCricinfo.

## A presentation format of this work is available [here](Presentation/WPL_Presentation.pdf).

You can learn about the structure of the Cricsheet files at
https://cricsheet.org/format/json/, and you can find the available downloads of Cricsheet data at https://cricsheet.org/downloads/, and the most up-to-date version at https://cricsheet.org/downloads/wpl_json.zip. For auction data, see https://www.espncricinfo.com/auction/wpl-2026-auction-1513120.


The matches contained in the season are listed below, as well as Cricsheet Match ID.

2026-02-05 - club - WPL - female - 1513703 - Delhi Capitals vs Royal Challengers Bengaluru

2026-02-03 - club - WPL - female - 1513702 - Gujarat Giants vs Delhi Capitals

2026-02-01 - club - WPL - female - 1513701 - UP Warriorz vs Delhi Capitals

2026-01-30 - club - WPL - female - 1513700 - Gujarat Giants vs Mumbai Indians

2026-01-29 - club - WPL - female - 1513699 - UP Warriorz vs Royal Challengers Bengaluru

2026-01-27 - club - WPL - female - 1513698 - Gujarat Giants vs Delhi Capitals

2026-01-26 - club - WPL - female - 1513697 - Mumbai Indians vs Royal Challengers Bengaluru

2026-01-24 - club - WPL - female - 1513696 - Royal Challengers Bengaluru vs Delhi Capitals

2026-01-22 - club - WPL - female - 1513695 - Gujarat Giants vs UP Warriorz

2026-01-20 - club - WPL - female - 1513694 - Mumbai Indians vs Delhi Capitals

2026-01-19 - club - WPL - female - 1513693 - Royal Challengers Bengaluru vs Gujarat Giants

2026-01-17 - club - WPL - female - 1513692 - Delhi Capitals vs Royal Challengers Bengaluru

2026-01-17 - club - WPL - female - 1513691 - UP Warriorz vs Mumbai Indians

2026-01-16 - club - WPL - female - 1513690 - Royal Challengers Bengaluru vs Gujarat Giants

2026-01-15 - club - WPL - female - 1513689 - Mumbai Indians vs UP Warriorz

2026-01-14 - club - WPL - female - 1513688 - UP Warriorz vs Delhi Capitals

2026-01-13 - club - WPL - female - 1513687 - Gujarat Giants vs Mumbai Indians

2026-01-12 - club - WPL - female - 1513686 - UP Warriorz vs Royal Challengers Bengaluru

2026-01-11 - club - WPL - female - 1513685 - Gujarat Giants vs Delhi Capitals

2026-01-10 - club - WPL - female - 1513684 - Mumbai Indians vs Delhi Capitals

2026-01-10 - club - WPL - female - 1513683 - Gujarat Giants vs UP Warriorz

2026-01-09 - club - WPL - female - 1513682 - Mumbai Indians vs Royal Challengers Bengaluru
