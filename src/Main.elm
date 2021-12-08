module Main exposing (..)

import Browser
import Browser.Events
import Ease
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Icons
import SmoothScroll
import Svg
import Svg.Attributes
import Task
import VirtualDom



---- MODEL ----


type alias ScreenSize =
    { width : Int
    , height : Int
    }


type Device
    = Desktop
    | Phone


type Tab
    = Portfolio
    | About
    | Contact


type Project
    = Roco
    | Honeysuckle
    | Luna
    | DailyUI
    | ContraryGarden
    | Misc


type alias Model =
    { screenSize : ScreenSize
    , device : Device
    , activeTab : Tab
    , hoveredTab : Maybe Tab
    , hoveredProject : Maybe Project
    }


type alias Flags =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { screenSize = ScreenSize flags.width flags.height
      , device = classifyDevice flags.width flags.height
      , activeTab = Portfolio
      , hoveredTab = Nothing
      , hoveredProject = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetScreenSize Int Int
    | SetDeviceClass Int Int
    | NavTo Tab
    | NavHover Tab
    | NavLeave
    | ProjectHover Project
    | ProjectLeave
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetScreenSize width height ->
            ( setScreenSize width height model, Cmd.none )

        SetDeviceClass width height ->
            ( setDeviceClass width height model, Cmd.none )

        NavTo tab ->
            ( navTo tab model, scrollToSection tab model )

        NavHover tab ->
            ( navHover tab model, Cmd.none )

        NavLeave ->
            ( navLeave model, Cmd.none )

        ProjectHover project ->
            ( projectHover project model, Cmd.none )

        ProjectLeave ->
            ( projectLeave model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


setScreenSize : Int -> Int -> Model -> Model
setScreenSize width height model =
    { model | screenSize = ScreenSize width height }


setDeviceClass : Int -> Int -> Model -> Model
setDeviceClass width height model =
    { model | device = classifyDevice width height }


navTo : Tab -> Model -> Model
navTo tab model =
    { model | activeTab = tab }


navHover : Tab -> Model -> Model
navHover tab model =
    { model | hoveredTab = Just tab }


navLeave : Model -> Model
navLeave model =
    { model | hoveredTab = Nothing }


projectHover : Project -> Model -> Model
projectHover project model =
    { model | hoveredProject = Just project }


projectLeave : Model -> Model
projectLeave model =
    { model | hoveredProject = Nothing }


classifyDevice : Int -> Int -> Device
classifyDevice width _ =
    if width < 700 then
        Phone

    else
        Desktop


scrollToSection : Tab -> Model -> Cmd Msg
scrollToSection tab model =
    case tab of
        Portfolio ->
            scrollTo 0

        About ->
            scrollTo <|
                toFloat <|
                    scaleFromWidth 0.45 model

        Contact ->
            scrollTo <|
                toFloat <|
                    scaleFromWidth 0.85 model


scrollTo : Float -> Cmd Msg
scrollTo y =
    let
        task =
            SmoothScroll.scrollTo <|
                SmoothScroll.createConfig
                    Ease.outCubic
                    400
    in
    Task.perform (always NoOp) (task y)



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize SetScreenSize
        , Browser.Events.onResize SetDeviceClass
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.device of
        Desktop ->
            viewDesktop model

        Phone ->
            viewPhone model


viewDesktop : Model -> Html Msg
viewDesktop model =
    layout
        [ width fill
        , inFront <| navBar model
        , inFront <| viewSidebar model
        ]
    <|
        row
            [ width fill
            , height fill
            , Background.color cream
            ]
            [ viewMain model ]


viewPhone : Model -> Html Msg
viewPhone model =
    layout [ width fill ] <|
        column [ width fill ]
            [ viewTopbar model
            , viewMain model
            ]


viewTopbar : Model -> Element Msg
viewTopbar model =
    row
        [ width fill
        , padding <| padSm model
        , spacing <| padSm model
        , Background.color darkBrown
        ]
        [ personName model
        , personLogo model
        , personTitle model
        ]


viewSocialLinks : Model -> Element Msg
viewSocialLinks model =
    row
        [ width fill
        , padding <| padLg model
        , alignBottom
        , Background.color darkBrown
        ]
        [ socialLinks model ]


viewSidebar : Model -> Element Msg
viewSidebar model =
    column
        [ width <| px <| scaleFromWidth 0.33 model
        , height fill
        , paddingEach
            { edges | top = padXxl model }
        , spacing <| padSm model
        , Background.color darkBrown
        ]
        [ personName model
        , personLogo model
        , personTitle model
        , socialLinks model
        ]


personName : Model -> Element Msg
personName model =
    el
        [ centerX
        , centerY
        , Font.size <| fontLg model
        , Font.color white
        , Font.family serif
        ]
    <|
        text "Mariaye Vickery"


personLogo : Model -> Element Msg
personLogo model =
    let
        color =
            toSvgColor brown

        height =
            String.fromInt <|
                scaleFromWidth 0.136 model
    in
    el [ centerX, centerY ] <|
        html <|
            Icons.logo
                [ Svg.Attributes.fill color
                , Svg.Attributes.height height
                ]


personTitle : Model -> Element Msg
personTitle model =
    let
        size =
            fontSm model

        spacing =
            toFloat size * 0.23
    in
    el
        [ centerX
        , centerY
        , Font.color white
        , Font.size size
        , Font.letterSpacing spacing
        , Font.family sansSerif
        ]
    <|
        text "JR. UI/UX DESIGNER"


socialLinks : Model -> Element Msg
socialLinks model =
    let
        height =
            String.fromInt <|
                socialIconHeight model

        socialIcon icon =
            el [] <|
                html <|
                    icon
                        [ Svg.Attributes.height height ]

        socialLink icon url =
            newTabLink []
                { url = url
                , label = socialIcon icon
                }
    in
    row
        [ centerX
        , centerY
        , paddingEach { edges | top = padXl model }
        , spacing <| padSm model
        ]
        [ socialLink Icons.linkedin "https://www.linkedin.com/in/mariayevickery"
        , socialLink Icons.dribbble "https://dribbble.com/marsvic"
        , socialLink Icons.behance "https://www.behance.net/mariayevickery"
        , socialLink Icons.instagram "https://www.instagram.com/marsviux"
        , socialLink Icons.twitter "https://twitter.com/marsviux"
        ]


navBar : Model -> Element Msg
navBar model =
    let
        padding =
            { edges
                | top = padXl model
                , bottom = padLg model
                , left = scaleFromWidth 0.33 model
            }
    in
    el
        [ width fill
        , paddingEach padding
        , Background.color creamTranslucent
        ]
    <|
        row
            [ centerX
            , spacing <| padXl model
            ]
            [ buttonNav "PORTFOLIO" Portfolio model
            , buttonNav "ABOUT" About model
            , buttonNav "CONTACT" Contact model
            ]


buttonNav : String -> Tab -> Model -> Element Msg
buttonNav label tab model =
    let
        isActiveTab =
            model.activeTab == tab

        isHoveredTab =
            case model.hoveredTab of
                Just hoveredTab ->
                    hoveredTab == tab

                Nothing ->
                    False

        fontColor =
            if isActiveTab || isHoveredTab then
                lightBrown

            else
                darkGrey

        size =
            fontMd model

        spacing =
            toFloat size * 0.15

        buttonLabel =
            el
                [ Font.size size
                , Font.color fontColor
                , Font.letterSpacing spacing
                , Font.family sansSerif
                , Events.onMouseEnter <| NavHover tab
                , Events.onMouseLeave NavLeave
                ]
            <|
                text label
    in
    Input.button [ focused [] ]
        { onPress = Just <| NavTo tab
        , label = buttonLabel
        }


viewMain : Model -> Element Msg
viewMain model =
    let
        pad =
            case model.device of
                Desktop ->
                    { edges | left = scaleFromWidth 0.33 model }

                Phone ->
                    edges
    in
    column
        [ width fill
        , alignTop
        , paddingEach pad
        , Background.color cream
        ]
        [ viewPortfolio model
        , viewAbout model
        , viewContact model
        ]


viewPortfolio : Model -> Element Msg
viewPortfolio model =
    case model.device of
        Desktop ->
            viewPortfolioDesktop model

        Phone ->
            viewPortfolioPhone model


viewPortfolioDesktop : Model -> Element Msg
viewPortfolioDesktop model =
    column
        [ width fill
        , paddingEach
            { edges | top = scaleFromWidth 0.125 model }
        ]
        [ sectionTitle "PROJECTS" model
        , projectsGridDesktop model
        ]


viewPortfolioPhone : Model -> Element Msg
viewPortfolioPhone model =
    column
        [ width fill
        , paddingEach
            { edges | top = padXl model }
        ]
        [ sectionTitle "PROJECTS" model
        , projectsGridPhone model
        ]


projectsGridDesktop : Model -> Element Msg
projectsGridDesktop model =
    let
        gridSpacing =
            padLg model

        gridRow =
            row [ spacing gridSpacing ]

        grid =
            column
                [ centerX
                , paddingEach
                    { edges | top = padXl model }
                , spacing gridSpacing
                ]

        dimension =
            projectSquareDimension model
    in
    grid
        [ gridRow
            [ squareRoco dimension model
            , squareHoneysuckle dimension model
            , squareLuna dimension model
            ]
        , gridRow
            [ squareDailyUI dimension model
            , squareContraryGarden dimension model
            , blankProjectSquare Misc dimension model
            ]
        ]


projectsGridPhone : Model -> Element Msg
projectsGridPhone model =
    let
        gridSpacing =
            padMd model

        gridRow =
            row [ spacing gridSpacing ]

        grid =
            column
                [ centerX
                , paddingEach
                    { edges | top = padXl model }
                , spacing gridSpacing
                ]

        dimension =
            projectSquareDimension model
    in
    grid
        [ gridRow
            [ squareRoco dimension model
            , squareHoneysuckle dimension model
            ]
        , gridRow
            [ squareLuna dimension model
            , squareDailyUI dimension model
            ]
        , gridRow
            [ squareContraryGarden dimension model
            , blankProjectSquare Misc dimension model
            ]
        ]


squareDailyUI : Int -> Model -> Element Msg
squareDailyUI =
    projectSquare
        Icons.dailyUI
        "https://www.behance.net/mariayevickery"
        "Daily UI Exercises"
        DailyUI


squareRoco : Int -> Model -> Element Msg
squareRoco =
    projectSquare
        Icons.roco
        "https://www.behance.net/gallery/131255429/Roco"
        "Concept, branding, mobile UI/UX"
        Roco


squareHoneysuckle : Int -> Model -> Element Msg
squareHoneysuckle =
    projectSquare
        Icons.honeysuckle
        "https://www.behance.net/gallery/131255777/Honeysuckle-Chopsaw"
        "Concept, branding, web and mobile UI/UX"
        Honeysuckle


squareLuna : Int -> Model -> Element Msg
squareLuna =
    projectSquare
        Icons.luna
        "https://www.behance.net/gallery/131256029/Luna"
        "Concept, branding, web and mobile UI/UX"
        Luna


squareContraryGarden : Int -> Model -> Element Msg
squareContraryGarden =
    projectSquare
        Icons.contraryGarden
        "https://society6.com/mariaye"
        "Concept, branding, web and mobile UI/UX"
        ContraryGarden


projectSquare :
    (List (VirtualDom.Attribute Msg) -> Svg.Svg Msg)
    -> String
    -> String
    -> Project
    -> Int
    -> Model
    -> Element Msg
projectSquare icon url description project dimension model =
    let
        isHovered =
            case model.hoveredProject of
                Just hoveredProject ->
                    hoveredProject == project

                Nothing ->
                    False

        overlay =
            if isHovered then
                el
                    [ width <| px dimension
                    , height <| px dimension
                    , Background.color
                        darkBrownTranslucent
                    ]
                <|
                    paragraph
                        [ centerX
                        , centerY
                        , padding <| scaleFromWidth 0.005 model
                        , Font.size <| fontSm model
                        , Font.color white
                        , Font.family sansSerif
                        ]
                        [ text description ]

            else
                none

        shadowY =
            toFloat <|
                scaleFromWidth 0.0028 model

        projectIcon =
            html <|
                icon
                    [ Svg.Attributes.height <|
                        String.fromInt dimension
                    ]
    in
    newTabLink
        [ width <| px dimension
        , height <| px dimension
        , Background.color lightBrownTranslucent
        , Border.shadow
            { offset = ( 0, shadowY )
            , size = 0
            , blur = shadowY
            , color = blackTranslucent
            }
        , Events.onMouseEnter <| ProjectHover project
        , Events.onMouseLeave ProjectLeave
        , Events.onClick ProjectLeave
        , inFront overlay
        ]
        { url = url
        , label = projectIcon
        }


blankProjectSquare : Project -> Int -> Model -> Element Msg
blankProjectSquare project dimension model =
    let
        isHovered =
            case model.hoveredProject of
                Just hoveredProject ->
                    hoveredProject == project

                Nothing ->
                    False

        overlay =
            if isHovered then
                el
                    [ width <| px dimension
                    , height <| px dimension
                    , Background.color
                        darkBrownTranslucent
                    ]
                <|
                    paragraph
                        [ centerX
                        , centerY
                        , padding <| scaleFromWidth 0.005 model
                        , Font.size <| fontSm model
                        , Font.color white
                        , Font.family sansSerif
                        ]
                        []

            else
                none

        shadowY =
            toFloat <|
                scaleFromWidth 0.0028 model
    in
    el
        [ width <| px dimension
        , height <| px dimension
        , Background.color lightBrownTranslucent
        , Border.shadow
            { offset = ( 0, shadowY )
            , size = 0
            , blur = shadowY
            , color = blackTranslucent
            }
        , Events.onMouseEnter <| ProjectHover project
        , Events.onMouseLeave ProjectLeave
        , Events.onClick ProjectLeave
        , inFront overlay
        ]
        none


viewAbout : Model -> Element Msg
viewAbout model =
    column
        [ width fill
        , paddingEach
            { edges | top = padXxl model }
        ]
        [ sectionTitle "ABOUT" model
        , about model
        ]


about : Model -> Element Msg
about model =
    case model.device of
        Desktop ->
            row
                [ width <| px <| sectionWidth model
                , centerX
                , paddingEach
                    { edges | top = padXl model }
                , spacing <| padLg model
                ]
                [ el
                    [ width <| fillPortion 1
                    , alignTop
                    ]
                  <|
                    image [ width fill ]
                        { src = "%PUBLIC_URL%/headshot.png"
                        , description = "Photo of Mariaye Vickery"
                        }
                , column [ width <| fillPortion 2 ]
                    [ el [ width fill ] <|
                        paragraph
                            [ spacing <| fontMd model
                            , Font.alignLeft
                            , Font.size <| fontMd model
                            , Font.family sansSerif
                            ]
                            [ text "I graduated from the University of Toronto in 2018 with a BA in Arts Management and Cultural Policy. Since then, I have worked in the arts, with a natural inclination toward design and new media. While working in art galleries, I found myself designing web presences and advising artists on their digital brands. This led me to pursue a career path in arts management and UX design." ]
                    , resumeButton model
                    ]
                ]

        Phone ->
            column
                [ width <| px <| sectionWidth model
                , centerX
                , paddingEach
                    { edges | top = padXl model }
                , spacing <| padLg model
                ]
                [ el [ width fill ] <|
                    image [ width fill ]
                        { src = "%PUBLIC_URL%/headshot.png"
                        , description = "Photo of Mariaye Vickery"
                        }
                , el [ alignTop ] <|
                    paragraph
                        [ spacing <| fontMd model
                        , Font.alignLeft
                        , Font.size <| fontMd model
                        , Font.family sansSerif
                        ]
                        [ text "I graduated from the University of Toronto in 2018 with a BA in Arts Management and Cultural Policy. Since then, I have worked in the arts, with a natural inclination toward design and new media. While working in art galleries, I found myself designing web presences and advising artists on their digital brands. This led me to pursue a career path in arts management and UX design." ]
                ]


resumeButton : Model -> Element Msg
resumeButton model =
    let
        label =
            el
                [ Font.alignLeft
                , Font.size <| fontMd model
                , Font.family sansSerif
                , Font.color lightBrown
                ]
            <|
                text "DOWNLOAD RESUME"
    in
    el [ paddingEach { edges | top = padLg model } ] <|
        downloadAs []
            { label = label
            , filename = "Mariaye Vickery's Resume"
            , url = "%PUBLIC_URL%/resume.pdf"
            }


viewContact : Model -> Element Msg
viewContact model =
    column
        [ width fill
        , paddingEach
            { edges | top = padXxl model }
        ]
        [ sectionTitle "CONTACT" model
        , contact model
        ]


contact : Model -> Element Msg
contact model =
    column
        [ centerX
        , spacing <| padLg model
        , paddingEach
            { edges
                | top = padXl model
                , bottom = scaleFromWidth 0.11 model
            }
        ]
        [ email model
        , linkedin model
        , dribbble model
        , behance model
        , instagram model
        , twitter model
        ]


email : Model -> Element Msg
email =
    contactUrl
        Icons.email
        "mailto:mariaye.vickery@gmail.com"
        "mariaye.vickery@gmail.com"
        True


linkedin : Model -> Element Msg
linkedin =
    contactUrl
        Icons.linkedin
        "http://linkedin.com/in/mariayevickery/"
        "linkedin.com/in/mariayevickery"
        True


instagram : Model -> Element Msg
instagram =
    contactUrl
        Icons.instagram
        "http://instagram.com/marsviux"
        "instagram.com/marsviux"
        True


twitter : Model -> Element Msg
twitter =
    contactUrl
        Icons.twitter
        "http://twitter.com/marsviux"
        "twitter.com/marsviux"
        True


behance : Model -> Element Msg
behance =
    contactUrl
        Icons.behance
        "http://behance.net/mariayevickery"
        "behance.net/mariayevickery"
        True


dribbble : Model -> Element Msg
dribbble =
    contactUrl
        Icons.dribbble
        "http://dribbble.com/marsvic"
        "dribbble.com/marsvic"
        True


contactUrl :
    (List (VirtualDom.Attribute Msg) -> Svg.Svg Msg)
    -> String
    -> String
    -> Bool
    -> Model
    -> Element Msg
contactUrl icon url label isLink model =
    let
        height =
            String.fromInt <|
                socialIconHeight model

        socialIcon =
            el [] <|
                html <|
                    icon
                        [ Svg.Attributes.height height ]

        link =
            if isLink then
                newTabLink []
                    { label =
                        el
                            [ Font.size <| fontMd model
                            , Font.family <| sansSerif
                            ]
                        <|
                            text label
                    , url = url
                    }

            else
                el
                    [ Font.size <| fontMd model
                    , Font.family <| sansSerif
                    ]
                <|
                    text label
    in
    row [ spacing <| padSm model ]
        [ socialIcon
        , link
        ]


sectionTitle : String -> Model -> Element Msg
sectionTitle title model =
    let
        size =
            fontXxl model

        spacing =
            toFloat size * 0.15
    in
    el
        [ centerX
        , Font.size size
        , Font.letterSpacing spacing
        , Font.family serif
        ]
    <|
        text title


socialIconHeight : Model -> Int
socialIconHeight model =
    case model.device of
        Desktop ->
            scaleFromWidth 0.02 model

        Phone ->
            scaleFromWidth 0.07 model


scaleFromWidth : Float -> Model -> Int
scaleFromWidth factor model =
    scale factor model.screenSize.width


scale : Float -> Int -> Int
scale factor number =
    number
        |> toFloat
        |> (*) factor
        |> round


projectSquareDimension : Model -> Int
projectSquareDimension model =
    case model.device of
        Desktop ->
            scaleFromWidth 0.136 model

        Phone ->
            scaleFromWidth 0.32 model


sectionWidth : Model -> Int
sectionWidth model =
    let
        squareWidth =
            projectSquareDimension model

        padding =
            padLg model
    in
    case model.device of
        Desktop ->
            3 * squareWidth + 2 * padding

        Phone ->
            2 * squareWidth + padding



---- COLOR ----


white : Color
white =
    rgb255 255 255 255


cream : Color
cream =
    rgb255 243 242 237


creamTranslucent : Color
creamTranslucent =
    rgba255 243 242 237 0.9


lightBrown : Color
lightBrown =
    rgb255 173 118 75


lightBrownTranslucent : Color
lightBrownTranslucent =
    rgba255 173 118 75 0.52


brown : Color
brown =
    rgb255 173 118 75


darkBrown : Color
darkBrown =
    rgb255 44 42 39


darkBrownTranslucent : Color
darkBrownTranslucent =
    rgba255 44 42 39 0.9


darkGrey : Color
darkGrey =
    rgb255 44 42 39


blackTranslucent : Color
blackTranslucent =
    rgba255 0 0 0 0.25


toSvgColor : Color -> String
toSvgColor color =
    let
        to255 accessor =
            toRgb color
                |> accessor
                |> (*) 255
                |> String.fromFloat
    in
    String.concat
        [ "rgb("
        , to255 .red
        , ","
        , to255 .green
        , ","
        , to255 .blue
        , ")"
        ]



---- PADDING ----


edges : { left : Int, right : Int, top : Int, bottom : Int }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


padXxl : Model -> Int
padXxl =
    scalePad << padXl


padXl : Model -> Int
padXl =
    scalePad << padLg


padLg : Model -> Int
padLg =
    scalePad << padMd


padMd : Model -> Int
padMd =
    scalePad << padSm


padSm : Model -> Int
padSm model =
    case model.device of
        Desktop ->
            scaleFromWidth 0.014 model

        Phone ->
            scaleFromWidth 0.05 model


scalePad : Int -> Int
scalePad =
    scale 1.5



---- FONT ----


fontXxl : Model -> Int
fontXxl =
    scaleFont << fontXl


fontXl : Model -> Int
fontXl =
    scaleFont << fontLg


fontLg : Model -> Int
fontLg =
    scaleFont << fontMd


fontMd : Model -> Int
fontMd =
    scaleFont << fontSm


fontSm : Model -> Int
fontSm model =
    case model.device of
        Desktop ->
            scaleFromWidth 0.01 model

        Phone ->
            scaleFromWidth 0.025 model


scaleFont : Int -> Int
scaleFont =
    scale 1.33


serif : List Font.Font
serif =
    [ Font.external
        { name = "Cormorant SC"
        , url = "https://fonts.googleapis.com/css?family=Cormorant SC"
        }
    , Font.serif
    ]


sansSerif : List Font.Font
sansSerif =
    [ Font.external
        { name = "Roboto"
        , url = "https://fonts.googleapis.com/css2?family=Roboto:wght@300&display=swap"
        }
    , Font.sansSerif
    ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
