module WebApp.StyleSheet (commonStyleSheet) where

import           Text.Internal.Css (Css)
import           Yesod             (cassius)

commonStyleSheet :: p -> Css
commonStyleSheet =
    [cassius|
        *
            font-family: var(--fontStack-monospace, ui-monospace, SFMono-Regular, SF Mono, Menlo, Consolas, Liberation Mono, monospace) !important

        body
            margin: 0 !important
            padding: 0 !important

        .mainAppHeader
            background-color: #DCDCDC
            width: 100%
            height: 5vw
            user-select: none
            font-size: 4vw

        .orderOptions
            background-color: #FFFFFF
            width: 100%
            height: 5vw
            user-select: none
            font-size: 3vw

        .centredText
            display: inline-block
            margin-top: calc((5vw - 3vw) / 2)
            margin-bottom: calc((5vw - 3vw) / 2)

        .button
            text-decoration: none
            padding-block: 0
            box-sizing: border-box
            border: 0.2vw solid #000000
            background-color: #FFFFFF
            font-size: 3.6vw
            width: 4em
            display: inline-block
            text-align: center
            margin-top: 2px
            margin-bottom: 2px
            margin-left: 2px
            margin-right: 2px
            user-select: none

        .noVerticalMargin
            margin-top: 0
            margin-bottom: 0

        .noHorizontalMargin
            margin-left: 0
            margin-right: 0

        .button:link,.button:visited,.button:hover
            color: #000000

        table
            border-collapse: collapse
            width: 100%
            font-size: 3vw
            table-layout: fixed

        th
            border: 1px solid #DDDDDD
            text-align: center
            padding: 8px
            user-select: none

        td
            border: 1px solid #DDDDDD
            padding: 8px
            overflow-x: auto

        .leftAlign
            text-align: left

        .centreAlign
            text-align: center

        tr:nth-child(even)
            background-color: #DDDDDD

        .shoppingListCheckbox
            align-items: center
            user-select: none
            box-sizing: border-box

        .shoppingListCheckbox label
            cursor: pointer
            display: flex

        .shoppingListCheckbox input
            cursor: pointer
            opacity: 0
            position: absolute
            display: none

        .shoppingListCheckbox label::before
            content: ''
            width: 5vw
            height: 5vw
            margin-left: auto
            margin-right: auto
            display: flex
            border: .1vw solid #000000

        .smaller label::before
            width: 4vw
            height: 4vw

        .shoppingListCheckbox input:checked+label::before
            content: '\002714'
            display: flex
            justify-content: center
            align-items: center
            color: #000000

        .itemDataInput
            box-sizing: border-box
            width: 100%
            height: 100%
            font-size: 3vw

        .spacer
            display: inline-block
            width: 3vw

        .selector
            font-size: 3vw
    |]
