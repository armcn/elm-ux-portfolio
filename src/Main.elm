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
import Html exposing (Html, img)
import Html.Attributes
import Html.Events
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


type Tab
    = Portfolio
    | About
    | Contact


type Project
    = DailyUI
    | Roco
    | Honeysuckle
    | Luna
    | ContraryGarden
    | Misc


type ProjectState
    = Hovered Project
    | Open Project
    | Closed


type Device
    = Desktop
    | Phone


type alias Model =
    { screenSize : ScreenSize
    , device : Device
    , activeTab : Tab
    , hoveredTab : Maybe Tab
    , projectState : ProjectState
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
      , projectState = Closed
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | SetScreenSize Int Int
    | SetDeviceClass Int Int
    | NavTo Tab
    | NavHover Tab
    | NavLeave
    | ProjectHover Project
    | ProjectLeave
    | OpenProject Project
    | GoHome


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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

        OpenProject project ->
            ( openProject project model, Cmd.none )

        GoHome ->
            ( goHome model, Cmd.none )


setScreenSize : Int -> Int -> Model -> Model
setScreenSize width height model =
    { model | screenSize = ScreenSize width height }


setDeviceClass : Int -> Int -> Model -> Model
setDeviceClass width height model =
    { model | device = classifyDevice width height }


classifyDevice : Int -> Int -> Device
classifyDevice width height =
    if width < 700 then
        Phone

    else
        Desktop


navTo : Tab -> Model -> Model
navTo tab model =
    { model | activeTab = tab }


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
                    scaleFromWidth 0.8 model


scrollTo : Float -> Cmd Msg
scrollTo y =
    let
        task =
            SmoothScroll.scrollTo <|
                SmoothScroll.createConfig
                    Ease.outCubic
                    300
    in
    Task.perform (always NoOp) (task y)


navHover : Tab -> Model -> Model
navHover tab model =
    { model | hoveredTab = Just tab }


navLeave : Model -> Model
navLeave model =
    { model | hoveredTab = Nothing }


projectHover : Project -> Model -> Model
projectHover project model =
    { model | projectState = Hovered project }


projectLeave : Model -> Model
projectLeave model =
    { model | projectState = Closed }


openProject : Project -> Model -> Model
openProject project model =
    { model | projectState = Open project }


goHome : Model -> Model
goHome model =
    { model | projectState = Closed }



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
        , inFront <| viewProject model
        ]
    <|
        row
            [ width fill
            , height fill
            , Background.color palette.cream
            ]
            [ viewMain model ]


viewPhone : Model -> Html Msg
viewPhone model =
    layout
        [ width fill
        , inFront <| viewSocialLinks model
        ]
    <|
        column [ width fill ]
            [ viewTopbar model
            , viewMain model
            ]


viewTopbar : Model -> Element Msg
viewTopbar model =
    row
        [ width fill
        , padding <| paddingSize.lg model
        , spacing <| paddingSize.md model
        , Background.color palette.darkBrown
        ]
        [ personName model
        , personLogo model
        , personTitle model
        ]


viewSocialLinks : Model -> Element Msg
viewSocialLinks model =
    row
        [ width fill
        , padding <| paddingSize.lg model
        , alignBottom
        , Background.color palette.darkBrown
        ]
        [ socialLinks model ]


viewProject : Model -> Element Msg
viewProject model =
    case model.projectState of
        Open project ->
            column
                [ width fill
                , height fill
                , padding <| paddingSize.xl model
                , Background.color palette.cream
                ]
                [ Input.button []
                    { onPress = Just GoHome
                    , label = text "Back"
                    }
                ]

        _ ->
            none


viewSidebar : Model -> Element Msg
viewSidebar model =
    column
        [ width <| px <| scaleFromWidth 0.33 model
        , height fill
        , paddingEach
            { edges | bottom = paddingSize.xxl model }
        , spacing <| paddingSize.lg model
        , Background.color palette.darkBrown
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
        , Font.size <| fontSize.lg model
        , Font.color palette.white
        , Font.family serif
        ]
    <|
        text "Mariaye Vickery"


personLogo : Model -> Element Msg
personLogo model =
    let
        color =
            toSvgColor palette.brown

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
            fontSize.sm model

        spacing =
            toFloat size * 0.15
    in
    el
        [ centerX
        , centerY
        , Font.color palette.white
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
        , spacing <| paddingSize.lg model
        ]
        [ socialLink Icons.dribbble "https://dribbble.com/marsvic"
        , socialLink Icons.behance "https://www.behance.net/mariayevickery"
        , socialLink Icons.linkedin "https://www.linkedin.com/in/mariayevickery"
        , socialLink Icons.instagram "https://www.instagram.com/marsviux"
        , socialLink Icons.twitter "https://twitter.com/marsviux"
        ]


navBar : Model -> Element Msg
navBar model =
    let
        padding =
            { edges
                | top = paddingSize.xl model
                , bottom = paddingSize.lg model
                , left = scaleFromWidth 0.33 model
            }
    in
    el
        [ width fill
        , paddingEach padding
        , Background.color palette.creamTranslucent
        ]
    <|
        row
            [ centerX
            , spacing <| paddingSize.xl model
            , Font.size <| fontSize.md model
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
                palette.lightBrown

            else
                palette.darkGrey

        size =
            fontSize.md model

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
        , Background.color palette.cream
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
            { edges | top = scaleFromWidth 0.1 model }
        ]
        [ sectionTitle "PROJECTS" model
        , projectsGridDesktop model
        ]


viewPortfolioPhone : Model -> Element Msg
viewPortfolioPhone model =
    column
        [ width fill
        , paddingEach
            { edges | top = scaleFromWidth 0.2 model }
        ]
        [ sectionTitle "PROJECTS" model
        , projectsGridPhone model
        ]


projectsGridDesktop : Model -> Element Msg
projectsGridDesktop model =
    let
        gridSpacing =
            paddingSize.lg model

        gridRow =
            row [ spacing gridSpacing ]

        grid =
            column
                [ centerX
                , paddingEach
                    { edges | top = paddingSize.xl model }
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
            , squareMisc dimension model
            ]
        ]


projectsGridPhone : Model -> Element Msg
projectsGridPhone model =
    let
        gridSpacing =
            paddingSize.lg model

        gridRow =
            row [ spacing gridSpacing ]

        grid =
            column
                [ centerX
                , paddingEach
                    { edges | top = paddingSize.xl model }
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
            , squareMisc dimension model
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


squareMisc : Int -> Model -> Element Msg
squareMisc =
    projectSquare
        Icons.misc
        ""
        "Miscellaneous designs"
        Misc


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
            case model.projectState of
                Hovered hoveredProject ->
                    hoveredProject == project

                _ ->
                    False

        overlay =
            if isHovered then
                el
                    [ width <| px dimension
                    , height <| px dimension
                    , Background.color
                        palette.darkBrownTranslucent
                    ]
                <|
                    paragraph
                        [ centerX
                        , centerY
                        , padding <| scaleFromWidth 0.005 model
                        , Font.size <| fontSize.sm model
                        , Font.color palette.white
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
        , Background.color palette.lightBrownTranslucent
        , Border.shadow
            { offset = ( 0, shadowY )
            , size = 0
            , blur = shadowY
            , color = palette.blackTranslucent
            }
        , Events.onMouseEnter <| ProjectHover project
        , Events.onMouseLeave ProjectLeave
        , focused []
        , inFront overlay
        ]
        { url = url
        , label = projectIcon
        }


viewAbout : Model -> Element Msg
viewAbout model =
    column
        [ width fill
        , paddingEach
            { edges | top = scaleFromWidth 0.1 model }
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
                    { edges | top = paddingSize.xl model }
                , spacing <| paddingSize.lg model
                ]
                [ el [ width <| fillPortion 1 ] <|
                    image [ width fill ]
                        { src = "%PUBLIC_URL%/headshot.png"
                        , description = "Photo of Mariaye Vickery"
                        }
                , el
                    [ width <| fillPortion 2
                    , alignTop
                    ]
                  <|
                    paragraph
                        [ spacing <| fontSize.md model
                        , Font.alignLeft
                        , Font.size <| fontSize.md model
                        , Font.family sansSerif
                        ]
                        [ text "I graduated from the University of Toronto in 2018 with a BA in Arts Management and Cultural Policy. Since then, I have worked in the arts, with a natural inclination toward design and new media. While working in art galleries, I found myself designing web presences and advising artists on their digital brands. This led me to pursue a career path in arts management and UX design." ]
                ]

        Phone ->
            column
                [ width <| px <| sectionWidth model
                , centerX
                , paddingEach
                    { edges | top = paddingSize.xl model }
                , spacing <| paddingSize.lg model
                ]
                [ el [ width fill ] <|
                    image [ width fill ]
                        { src = "%PUBLIC_URL%/headshot.png"
                        , description = "Photo of Mariaye Vickery"
                        }
                , el [ alignTop ] <|
                    paragraph
                        [ spacing <| fontSize.md model
                        , Font.alignLeft
                        , Font.size <| fontSize.md model
                        , Font.family sansSerif
                        ]
                        [ text "I graduated from the University of Toronto in 2018 with a BA in Arts Management and Cultural Policy. Since then, I have worked in the arts, with a natural inclination toward design and new media. While working in art galleries, I found myself designing web presences and advising artists on their digital brands. This led me to pursue a career path in arts management and UX design." ]
                ]


viewContact : Model -> Element Msg
viewContact model =
    column
        [ width fill
        , height <| px 1000
        , paddingEach
            { edges | top = scaleFromWidth 0.1 model }
        ]
        [ sectionTitle "CONTACT" model
        ]


sectionTitle : String -> Model -> Element Msg
sectionTitle title model =
    let
        size =
            fontSize.xxl model

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
            scaleFromWidth 0.0138 model

        Phone ->
            scaleFromWidth 0.1 model


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
            paddingSize.lg model
    in
    case model.device of
        Desktop ->
            3 * squareWidth + 2 * padding

        Phone ->
            2 * squareWidth + padding


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


palette =
    { white = rgb255 255 255 255
    , cream = rgb255 243 242 237
    , creamTranslucent = rgba255 243 242 237 0.9
    , lightBrown = rgb255 173 118 75
    , lightBrownTranslucent = rgba255 173 118 75 0.52
    , brown = rgb255 173 118 75
    , darkBrown = rgb255 44 42 39
    , darkBrownTranslucent = rgba255 44 42 39 0.9
    , darkGrey = rgb255 44 42 39
    , blackTranslucent = rgba255 0 0 0 0.25
    }


paddingSize =
    { xxl = scale 4 << paddingSizeBase
    , xl = scale 3 << paddingSizeBase
    , lg = paddingSizeBase
    , md = scale 0.7 << paddingSizeBase
    , sm = scale 0.5 << paddingSizeBase
    }


paddingSizeBase : Model -> Int
paddingSizeBase model =
    case model.device of
        Desktop ->
            scaleFromWidth 0.014 model

        Phone ->
            scaleFromWidth 0.05 model


edges : { left : Int, right : Int, top : Int, bottom : Int }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


fontSize =
    { xxl = scale 2 << fontSizeBase
    , lg = fontSizeBase
    , md = scale 0.7 << fontSizeBase
    , sm = scale 0.5 << fontSizeBase
    }


fontSizeBase : Model -> Int
fontSizeBase model =
    case model.device of
        Desktop ->
            scaleFromWidth 0.017 model

        Phone ->
            scaleFromWidth 0.04 model


serif : List Font.Font
serif =
    [ Font.external
        { name = "Cormorant SC"
        , url = "https://fonts.googleapis.com/css?family=Cormorant SC"
        }
    , Font.sansSerif
    ]


sansSerif : List Font.Font
sansSerif =
    [ Font.external
        { name = "Roboto"
        , url = "https://fonts.googleapis.com/css?family=Roboto"
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
