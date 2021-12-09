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
import Icons exposing (Icon)
import SmoothScroll
import Svg
import Svg.Attributes
import Task



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
    | None


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
        [ inFront <| navBar model
        , inFront <| sidebar model
        ]
    <|
        row
            [ width fill
            , height fill
            , Background.color cream
            ]
            [ mainArea Desktop model ]


viewPhone : Model -> Html Msg
viewPhone model =
    layout [] <|
        column [ width fill ]
            [ topbar model
            , mainArea Phone model
            ]


navBar : Model -> Element Msg
navBar model =
    let
        pads =
            { edges
                | top = padLg model
                , bottom = padLg model
                , left = oneThirdWidth model
            }
    in
    el
        [ width fill
        , paddingEach pads
        , Background.color creamTranslucent
        ]
    <|
        row
            [ centerX
            , spacing <| padXl model
            ]
            [ navPortfolio model
            , navAbout model
            , navContact model
            ]


navPortfolio : Model -> Element Msg
navPortfolio =
    navButton "PORTFOLIO" Portfolio


navAbout : Model -> Element Msg
navAbout =
    navButton "ABOUT" About


navContact : Model -> Element Msg
navContact =
    navButton "CONTACT" Contact


navButton : String -> Tab -> Model -> Element Msg
navButton label tab model =
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

        fontSize =
            fontMd model

        letterSpacing =
            toFloat fontSize * 0.15

        buttonLabel =
            el
                [ Font.size fontSize
                , Font.letterSpacing letterSpacing
                , Font.color fontColor
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


topbar : Model -> Element Msg
topbar model =
    row
        [ width fill
        , height <| px <| padXxl model
        , padding <| padSm model
        , spacing <| padSm model
        , Background.color darkBrown
        ]
        [ personName Phone model
        , personLogo
        , personJobTitle Phone model
        ]


sidebar : Model -> Element Msg
sidebar model =
    el
        [ width <| px <| oneThirdWidth model
        , height fill
        , Background.color darkBrown
        ]
    <|
        column
            [ width fill
            , height fill
            , spacing <| padXxl model
            ]
            [ column
                [ centerX
                , centerY
                , spacing <| padSm model
                ]
                [ personName Desktop model
                , personLogo
                , personJobTitle Desktop model
                ]
            , socialLinks model
            ]


personName : Device -> Model -> Element Msg
personName device model =
    let
        fontSize =
            case device of
                Desktop ->
                    fontLg model

                Phone ->
                    fontMd model
    in
    el
        [ centerX
        , Font.size fontSize
        , Font.color white
        , Font.family serif
        ]
    <|
        text "Mariaye Vickery"


personLogo : Element Msg
personLogo =
    el
        [ width fill
        , height fill
        , centerX
        ]
    <|
        html <|
            Icons.logo
                [ Svg.Attributes.fill <|
                    toSvgColor brown
                ]


personJobTitle : Device -> Model -> Element Msg
personJobTitle device model =
    let
        fontSize =
            fontSm model

        letterSpacing =
            case device of
                Desktop ->
                    toFloat fontSize * 0.23

                Phone ->
                    0
    in
    el
        [ centerX
        , centerY
        , Font.color white
        , Font.size fontSize
        , Font.letterSpacing letterSpacing
        , Font.family sansSerif
        ]
    <|
        text "JR. UI/UX DESIGNER"


socialLinks : Model -> Element Msg
socialLinks model =
    row
        [ centerX
        , centerY
        , spacing <| padSm model
        ]
        [ linkedinIconLink model
        , dribbbleIconLink model
        , behanceIconLink model
        , instagramIconLink model
        , twitterIconLink model
        ]


linkedinIconLink : Model -> Element Msg
linkedinIconLink =
    socialLink Icons.linkedin linkedinUrl


dribbbleIconLink : Model -> Element Msg
dribbbleIconLink =
    socialLink Icons.dribbble dribbbleUrl


behanceIconLink : Model -> Element Msg
behanceIconLink =
    socialLink Icons.behance behanceUrl


instagramIconLink : Model -> Element Msg
instagramIconLink =
    socialLink Icons.instagram instagramUrl


twitterIconLink : Model -> Element Msg
twitterIconLink =
    socialLink Icons.twitter twitterUrl


socialLink : Icon Msg -> String -> Model -> Element Msg
socialLink icon url model =
    let
        height =
            String.fromInt <|
                socialIconHeight model

        socialIcon =
            html <|
                icon
                    [ Svg.Attributes.height height ]
    in
    newTabLink []
        { url = url
        , label = socialIcon
        }


mainArea : Device -> Model -> Element Msg
mainArea device model =
    let
        pads =
            case device of
                Desktop ->
                    { edges | left = oneThirdWidth model }

                Phone ->
                    edges
    in
    column
        [ width fill
        , paddingEach pads
        , Background.color cream
        ]
        [ portfolio device model
        , about model
        , contact model
        ]


portfolio : Device -> Model -> Element Msg
portfolio device model =
    let
        pads =
            case device of
                Desktop ->
                    { edges | top = scaleFromWidth 0.125 model }

                Phone ->
                    { edges | top = padXl model }
    in
    column
        [ width fill
        , paddingEach pads
        ]
        [ sectionTitle "PROJECTS" model
        , projectsGrid device model
        ]


projectsGrid : Device -> Model -> Element Msg
projectsGrid device model =
    let
        gridSpacing =
            case device of
                Desktop ->
                    padLg model

                Phone ->
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
            squareProjectDimension model

        roco =
            squareRoco dimension model

        honeysuckle =
            squareHoneysuckle dimension model

        luna =
            squareLuna dimension model

        dailyUI =
            squareDailyUI dimension model

        contraryGarden =
            squareContraryGarden dimension model

        blank =
            squareBlank dimension model
    in
    grid <|
        case device of
            Desktop ->
                [ gridRow [ roco, honeysuckle, luna ]
                , gridRow [ dailyUI, contraryGarden, blank ]
                ]

            Phone ->
                [ gridRow [ roco, honeysuckle ]
                , gridRow [ luna, dailyUI ]
                , gridRow [ contraryGarden, blank ]
                ]


squareRoco : Int -> Model -> Element Msg
squareRoco =
    squareProject
        Icons.roco
        "https://www.behance.net/gallery/131255429/Roco"
        "Concept, branding, mobile UI/UX"
        Roco


squareHoneysuckle : Int -> Model -> Element Msg
squareHoneysuckle =
    squareProject
        Icons.honeysuckle
        "https://www.behance.net/gallery/131255777/Honeysuckle-Chopsaw"
        "Concept, branding, web and mobile UI/UX"
        Honeysuckle


squareLuna : Int -> Model -> Element Msg
squareLuna =
    squareProject
        Icons.luna
        "https://www.behance.net/gallery/131256029/Luna"
        "Concept, branding, web and mobile UI/UX"
        Luna


squareDailyUI : Int -> Model -> Element Msg
squareDailyUI =
    squareProject
        Icons.dailyUI
        "https://www.behance.net/mariayevickery"
        "Daily UI Exercises"
        DailyUI


squareContraryGarden : Int -> Model -> Element Msg
squareContraryGarden =
    squareProject
        Icons.contraryGarden
        "https://society6.com/mariaye"
        "Concept, branding, web and mobile UI/UX"
        ContraryGarden


squareBlank : Int -> Model -> Element Msg
squareBlank =
    squareProject
        (\_ -> Svg.svg [] [])
        ""
        ""
        None


squareProject : Icon Msg -> String -> String -> Project -> Int -> Model -> Element Msg
squareProject icon url description project dimension model =
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
                    , padding <| padXs model
                    , Background.color
                        darkBrownTranslucent
                    ]
                <|
                    paragraph
                        [ centerX
                        , centerY
                        , Font.size <| fontSm model
                        , Font.color white
                        , Font.family sansSerif
                        ]
                        [ text description ]

            else
                none

        projectIcon =
            html <|
                icon
                    [ Svg.Attributes.height <|
                        String.fromInt dimension
                    ]

        shadowY =
            toFloat <|
                scaleFromWidth 0.006 model
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


about : Model -> Element Msg
about model =
    let
        content =
            case model.device of
                Desktop ->
                    aboutDesktop model

                Phone ->
                    aboutPhone model
    in
    column
        [ centerX
        , paddingEach { edges | top = padXxl model }
        ]
        [ sectionTitle "ABOUT" model
        , content
        ]


aboutDesktop : Model -> Element Msg
aboutDesktop model =
    row
        [ width <| px <| sectionWidth model
        , centerX
        , paddingEach { edges | top = padXl model }
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
        , column
            [ width <| fillPortion 2
            , spacing <| padLg model
            ]
            [ paragraph
                [ spacing <| fontMd model
                , Font.alignLeft
                , Font.size <| fontMd model
                , Font.family sansSerif
                ]
                [ text aboutText ]
            , resumeButton model
            ]
        ]


aboutPhone : Model -> Element Msg
aboutPhone model =
    column
        [ width <| px <| sectionWidth model
        , centerX
        , paddingEach { edges | top = padXl model }
        , spacing <| padLg model
        ]
        [ image [ width fill ]
            { src = "%PUBLIC_URL%/headshot.png"
            , description = "Photo of Mariaye Vickery"
            }
        , paragraph
            [ alignTop
            , spacing <| fontMd model
            , Font.alignLeft
            , Font.size <| fontMd model
            , Font.family sansSerif
            ]
            [ text aboutText ]
        , resumeButton model
        ]


aboutText : String
aboutText =
    "I graduated from the University of Toronto in 2018 with a BA in Arts Management and Cultural Policy. Since then, I have worked in the arts, with a natural inclination toward design and new media. While working in art galleries, I found myself designing web presences and advising artists on their digital brands. This led me to pursue a career path in arts management and UX design."


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
    downloadAs []
        { label = label
        , filename = "Mariaye Vickery's Resume"
        , url = "%PUBLIC_URL%/resume.pdf"
        }


contact : Model -> Element Msg
contact model =
    let
        content =
            column
                [ spacing <| padLg model
                , paddingEach
                    { edges
                        | top = padXl model
                        , bottom = scaleFromWidth 0.11 model
                    }
                ]
                [ emailContactLink model
                , linkedinContactLink model
                , dribbbleContactLink model
                , behanceContactLink model
                , instagramContactLink model
                , twitterContactLink model
                ]
    in
    column
        [ centerX
        , paddingEach { edges | top = padXxl model }
        ]
        [ sectionTitle "CONTACT" model
        , content
        ]


emailContactLink : Model -> Element Msg
emailContactLink =
    contactLink
        Icons.email
        "mailto:mariaye.vickery@gmail.com"
        "mariaye.vickery@gmail.com"


linkedinContactLink : Model -> Element Msg
linkedinContactLink =
    contactLink
        Icons.linkedin
        linkedinUrl
        "linkedin.com/in/mariayevickery"


instagramContactLink : Model -> Element Msg
instagramContactLink =
    contactLink
        Icons.instagram
        instagramUrl
        "instagram.com/marsviux"


twitterContactLink : Model -> Element Msg
twitterContactLink =
    contactLink
        Icons.twitter
        twitterUrl
        "twitter.com/marsviux"


behanceContactLink : Model -> Element Msg
behanceContactLink =
    contactLink
        Icons.behance
        behanceUrl
        "behance.net/mariayevickery"


dribbbleContactLink : Model -> Element Msg
dribbbleContactLink =
    contactLink
        Icons.dribbble
        dribbbleUrl
        "dribbble.com/marsvic"


contactLink : Icon Msg -> String -> String -> Model -> Element Msg
contactLink icon url label model =
    let
        height =
            String.fromInt <|
                socialIconHeight model

        socialIcon =
            el [] <|
                html <|
                    icon
                        [ Svg.Attributes.height height ]

        linkText =
            el
                [ Font.size <| fontMd model
                , Font.family <| sansSerif
                ]
            <|
                text label

        linkLabel =
            row [ spacing <| padSm model ]
                [ socialIcon, linkText ]
    in
    newTabLink []
        { label = linkLabel
        , url = url
        }


sectionTitle : String -> Model -> Element Msg
sectionTitle title model =
    let
        fontSize =
            fontXxl model

        letterSpacing =
            toFloat fontSize * 0.15
    in
    el
        [ centerX
        , Font.size fontSize
        , Font.letterSpacing letterSpacing
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


oneThirdWidth : Model -> Int
oneThirdWidth =
    scaleFromWidth (1 / 3)


scaleFromWidth : Float -> Model -> Int
scaleFromWidth factor model =
    scale factor model.screenSize.width


scale : Float -> Int -> Int
scale factor number =
    number
        |> toFloat
        |> (*) factor
        |> round


squareProjectDimension : Model -> Int
squareProjectDimension model =
    case model.device of
        Desktop ->
            scaleFromWidth 0.136 model

        Phone ->
            scaleFromWidth 0.32 model


sectionWidth : Model -> Int
sectionWidth model =
    let
        squareWidth =
            squareProjectDimension model

        padding =
            padLg model
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


white : Color
white =
    rgb255 255 255 255


cream : Color
cream =
    rgb255 243 242 237


creamTranslucent : Color
creamTranslucent =
    rgba255 243 242 237 0.92


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


padXs : Model -> Int
padXs model =
    round <| toFloat (padSm model) / 1.5


scalePad : Int -> Int
scalePad =
    scale 1.5


edges : { left : Int, right : Int, top : Int, bottom : Int }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


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
            scaleFromWidth 0.03 model


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


linkedinUrl : String
linkedinUrl =
    "https://www.linkedin.com/in/mariayevickery"


dribbbleUrl : String
dribbbleUrl =
    "https://dribbble.com/marsvic"


behanceUrl : String
behanceUrl =
    "https://www.behance.net/mariayevickery"


instagramUrl : String
instagramUrl =
    "https://www.instagram.com/marsviux"


twitterUrl : String
twitterUrl =
    "https://twitter.com/marsviux"



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
