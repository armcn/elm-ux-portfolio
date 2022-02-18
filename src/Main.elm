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
import Http
import Icons exposing (Icon)
import Json.Encode as Encode
import SmoothScroll
import Svg.Attributes
import Task
import Validate



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
    | Cleaning


type alias ContactForm =
    { firstName : String
    , lastName : String
    , emailAddress : String
    , emailMessage : String
    }


type FormSelection
    = FirstName
    | LastName
    | EmailAddress
    | EmailMessage


type EmailFormState
    = EmptyEmail
    | ValidEmail
    | InvalidEmail


type SubmitButtonState
    = Unsubmitted
    | SubmitHovered
    | SubmitPressed
    | Submitted


type alias Model =
    { screenSize : ScreenSize
    , device : Device
    , activeTab : Tab
    , hoveredTab : Maybe Tab
    , hoveredProject : Maybe Project
    , contactForm : ContactForm
    , formSelection : Maybe FormSelection
    , emailFormState : EmailFormState
    , invalidSubmission : Bool
    , submitButtonState : SubmitButtonState
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { screenSize = ScreenSize flags.width flags.height
      , device = classifyDevice flags.width flags.height
      , activeTab = Portfolio
      , hoveredTab = Nothing
      , hoveredProject = Nothing
      , contactForm = ContactForm "" "" "" ""
      , formSelection = Nothing
      , emailFormState = EmptyEmail
      , invalidSubmission = False
      , submitButtonState = Unsubmitted
      }
    , Cmd.none
    )


type alias Flags =
    { width : Int
    , height : Int
    }



---- UPDATE ----


type SubmitAction
    = HoverSubmit
    | LeaveSubmit
    | PressSubmit
    | UnpressSubmit


type Msg
    = SetScreenSize Int Int
    | SetDeviceClass Int Int
    | NavigateTo Tab
    | HoverNavItem Tab
    | LeaveNavItem
    | HoverProject Project
    | LeaveProject
    | UpdateForm FormSelection String
    | UpdateSubmit SubmitAction
    | Uploaded (Result Http.Error ())
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetScreenSize width height ->
            ( setScreenSize width height model, Cmd.none )

        SetDeviceClass width height ->
            ( setDeviceClass width height model, Cmd.none )

        NavigateTo tab ->
            ( navigateTo tab model, scrollToSection tab model )

        HoverNavItem tab ->
            ( hoverNavItem tab model, Cmd.none )

        LeaveNavItem ->
            ( leaveNavItem model, Cmd.none )

        HoverProject project ->
            ( hoverProject project model, Cmd.none )

        LeaveProject ->
            ( leaveProject model, Cmd.none )

        UpdateForm formSelection textInput ->
            ( updateForm formSelection textInput model, Cmd.none )

        UpdateSubmit submitAction ->
            updateSubmit submitAction model

        Uploaded _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


setScreenSize : Int -> Int -> Model -> Model
setScreenSize width height model =
    { model | screenSize = ScreenSize width height }


setDeviceClass : Int -> Int -> Model -> Model
setDeviceClass width height model =
    { model | device = classifyDevice width height }


navigateTo : Tab -> Model -> Model
navigateTo tab model =
    { model | activeTab = tab }


hoverNavItem : Tab -> Model -> Model
hoverNavItem tab model =
    { model | hoveredTab = Just tab }


leaveNavItem : Model -> Model
leaveNavItem model =
    { model | hoveredTab = Nothing }


hoverProject : Project -> Model -> Model
hoverProject project model =
    { model | hoveredProject = Just project }


leaveProject : Model -> Model
leaveProject model =
    { model | hoveredProject = Nothing }


updateForm : FormSelection -> String -> Model -> Model
updateForm formSelection textInput model =
    let
        contactForm =
            model.contactForm

        newContactForm =
            case formSelection of
                FirstName ->
                    { contactForm | firstName = textInput }

                LastName ->
                    { contactForm | lastName = textInput }

                EmailAddress ->
                    { contactForm | emailAddress = textInput }

                EmailMessage ->
                    { contactForm | emailMessage = textInput }
    in
    validateEmail
        { model
            | contactForm = newContactForm
            , submitButtonState = Unsubmitted
        }


updateSubmit : SubmitAction -> Model -> ( Model, Cmd Msg )
updateSubmit submitAction model =
    case submitAction of
        HoverSubmit ->
            ( hoverSubmit model, Cmd.none )

        LeaveSubmit ->
            ( leaveSubmit model, Cmd.none )

        PressSubmit ->
            ( pressSubmit model, Cmd.none )

        UnpressSubmit ->
            unpressSubmit model


hoverSubmit : Model -> Model
hoverSubmit model =
    case model.submitButtonState of
        Submitted ->
            model

        _ ->
            { model | submitButtonState = SubmitHovered }


leaveSubmit : Model -> Model
leaveSubmit model =
    case model.submitButtonState of
        Submitted ->
            model

        _ ->
            { model | submitButtonState = Unsubmitted }


pressSubmit : Model -> Model
pressSubmit model =
    let
        alreadySubmitted =
            model.submitButtonState == Submitted

        emptyEmail =
            model.emailFormState == EmptyEmail
    in
    if alreadySubmitted || emptyEmail then
        model

    else if model.emailFormState == InvalidEmail then
        { model | invalidSubmission = True }

    else
        { model | submitButtonState = SubmitPressed }


unpressSubmit : Model -> ( Model, Cmd Msg )
unpressSubmit model =
    case model.emailFormState of
        ValidEmail ->
            ( { model
                | submitButtonState = Submitted
                , invalidSubmission = False
                , contactForm = ContactForm "" "" "" ""
              }
            , postContact model.contactForm
            )

        _ ->
            ( { model | submitButtonState = Unsubmitted }, Cmd.none )


postContact : ContactForm -> Cmd Msg
postContact contactForm =
    Http.post
        { url = "https://contact.mariayevickery.com/contact/contact"
        , body = Http.jsonBody <| contactFormEncoder contactForm
        , expect = Http.expectWhatever Uploaded
        }


contactFormEncoder : ContactForm -> Encode.Value
contactFormEncoder contactForm =
    Encode.object
        [ ( "firstName", Encode.string contactForm.firstName )
        , ( "lastName", Encode.string contactForm.lastName )
        , ( "emailAddress", Encode.string contactForm.emailAddress )
        , ( "emailMessage", Encode.string contactForm.emailMessage )
        ]


validateEmail : Model -> Model
validateEmail model =
    case Validate.validate emailValidator model of
        Ok _ ->
            { model | emailFormState = ValidEmail }

        Err _ ->
            if String.isEmpty model.contactForm.emailAddress then
                { model | emailFormState = EmptyEmail }

            else
                { model | emailFormState = InvalidEmail }


emailValidator : Validate.Validator String Model
emailValidator =
    Validate.ifInvalidEmail
        (.emailAddress << .contactForm)
        (always "Please enter a valid email address.")


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
                , Events.onMouseEnter <| HoverNavItem tab
                , Events.onMouseLeave LeaveNavItem
                ]
            <|
                text label
    in
    Input.button [ focused [] ]
        { onPress = Just <| NavigateTo tab
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
        , personLogo model
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
                , personLogo model
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


personLogo : Model -> Element Msg
personLogo model =
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
                , Svg.Attributes.height <|
                    String.fromInt <|
                        scaleFromWidth 0.136 model
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

        cleaning =
            squareCleaning dimension model
    in
    grid <|
        case device of
            Desktop ->
                [ gridRow [ cleaning, roco, honeysuckle ]
                , gridRow [ luna, dailyUI, contraryGarden ]
                ]

            Phone ->
                [ gridRow [ cleaning, roco ]
                , gridRow [ honeysuckle, luna ]
                , gridRow [ dailyUI, contraryGarden ]
                ]


squareRoco : Int -> Model -> Element Msg
squareRoco =
    squareProject
        Icons.roco
        "https://www.behance.net/gallery/131255429/Roco"
        "Case study: Roco"
        "Concept, branding, mobile UI/UX"
        Roco


squareHoneysuckle : Int -> Model -> Element Msg
squareHoneysuckle =
    squareProject
        Icons.honeysuckle
        "https://www.behance.net/gallery/131255777/Honeysuckle-Chopsaw"
        "Case study: Wedding venue"
        "Concept, branding, web and mobile UI/UX"
        Honeysuckle


squareLuna : Int -> Model -> Element Msg
squareLuna =
    squareProject
        Icons.luna
        "https://www.behance.net/gallery/131256029/Luna"
        "Case study: Women's health"
        "Concept, branding, web and mobile UI/UX"
        Luna


squareDailyUI : Int -> Model -> Element Msg
squareDailyUI =
    squareProject
        Icons.dailyUI
        "https://www.behance.net/mariayevickery"
        "Daily UI Exercises"
        "UI concepts and practice"
        DailyUI


squareContraryGarden : Int -> Model -> Element Msg
squareContraryGarden =
    squareProject
        Icons.contraryGarden
        "https://society6.com/mariaye"
        "Contrary Garden"
        "Graphic design and artwork"
        ContraryGarden


squareCleaning : Int -> Model -> Element Msg
squareCleaning =
    squareProject
        Icons.cleaning
        "https://www.behance.net/gallery/135853731/Shauna-Loves-Cleaning"
        "Shauna Loves Cleaning"
        "UI/UX Design, Web Development and Branding"
        Cleaning


squareProject : Icon Msg -> String -> String -> String -> Project -> Int -> Model -> Element Msg
squareProject icon url heading description project dimension model =
    let
        isHovered =
            case model.hoveredProject of
                Just hoveredProject ->
                    hoveredProject == project

                Nothing ->
                    False

        overlay =
            if isHovered then
                column
                    [ width <| px dimension
                    , height <| px dimension
                    , padding <| padXs model
                    , spacing <| padSm model
                    , Background.color
                        darkBrownTranslucent
                    ]
                <|
                    [ el
                        [ centerX
                        , centerY
                        , Font.size <| fontSm model
                        , Font.color white
                        , Font.bold
                        , Font.family sansSerif
                        ]
                      <|
                        text heading
                    , paragraph
                        [ centerX
                        , centerY
                        , Font.size <| fontSm model
                        , Font.color white
                        , Font.family sansSerif
                        ]
                        [ text description ]
                    ]

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
        , Events.onMouseEnter <| HoverProject project
        , Events.onMouseLeave LeaveProject
        , Events.onClick LeaveProject
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
        shadowY =
            toFloat <|
                scaleFromWidth 0.006 model

        borderShadow =
            { offset = ( 0, shadowY )
            , size = 0
            , blur = shadowY
            , color = blackTranslucent
            }

        contactForm =
            column
                [ width fill
                , spacing <| padMd model
                , padding <| padMd model
                , Background.color lightBrownTranslucent
                , Border.shadow borderShadow
                ]
                [ row
                    [ width fill
                    , spacing <| padMd model
                    ]
                    [ firstNameInput model
                    , lastNameInput model
                    ]
                , emailInput model
                , messageInput model
                , submitButton model
                ]
    in
    column
        [ width <| px <| sectionWidth model
        , centerX
        , spacing <| padXl model
        , paddingEach
            { edges
                | top = padXxl model
                , bottom = scaleFromWidth 0.07 model
            }
        ]
        [ sectionTitle "CONTACT" model
        , contactForm
        ]


firstNameInput : Model -> Element Msg
firstNameInput model =
    Input.text
        (formInputStyle model)
        { onChange = UpdateForm FirstName
        , text = model.contactForm.firstName
        , placeholder = contactFormPlaceholder "First" model
        , label = Input.labelHidden "Enter your first name"
        }


lastNameInput : Model -> Element Msg
lastNameInput model =
    Input.text
        (formInputStyle model)
        { onChange = UpdateForm LastName
        , text = model.contactForm.lastName
        , placeholder = contactFormPlaceholder "Last" model
        , label = Input.labelHidden "Enter your last name"
        }


emailInput : Model -> Element Msg
emailInput model =
    column [ width fill ]
        [ Input.email
            (formInputStyle model)
            { onChange = UpdateForm EmailAddress
            , text = model.contactForm.emailAddress
            , placeholder = contactFormPlaceholder "Email" model
            , label = Input.labelHidden "Enter your email"
            }
        , invalidEmailMessage model
        ]


invalidEmailMessage : Model -> Element Msg
invalidEmailMessage model =
    let
        invalidSubmission =
            model.invalidSubmission

        invalidEmail =
            model.emailFormState == InvalidEmail
    in
    if invalidSubmission && invalidEmail then
        el
            [ paddingEach { edges | top = padSm model }
            , Font.family sansSerifBold
            , Font.size <| fontMd model
            , Font.color red
            ]
        <|
            text "Please enter a valid email address."

    else
        none


messageInput : Model -> Element Msg
messageInput model =
    Input.multiline
        (formInputStyle model ++ [ height <| px <| padXxl model ])
        { onChange = UpdateForm EmailMessage
        , text = model.contactForm.emailMessage
        , placeholder = contactFormPlaceholder "Message" model
        , label = Input.labelHidden "Enter your message"
        , spellcheck = True
        }


submitButton : Model -> Element Msg
submitButton model =
    let
        backgroundColor =
            case model.submitButtonState of
                Unsubmitted ->
                    darkBrown

                SubmitHovered ->
                    darkBrown

                SubmitPressed ->
                    white

                Submitted ->
                    rgba255 0 0 0 0

        fontColor =
            case model.submitButtonState of
                Unsubmitted ->
                    white

                SubmitHovered ->
                    white

                SubmitPressed ->
                    darkBrown

                Submitted ->
                    darkBrown

        shadowY =
            toFloat <|
                scaleFromWidth 0.006 model

        borderShadow =
            case model.submitButtonState of
                Unsubmitted ->
                    { offset = ( 0, shadowY )
                    , size = 0
                    , blur = shadowY
                    , color = blackTranslucent
                    }

                SubmitHovered ->
                    { offset = ( 0, shadowY * 1.3 )
                    , size = 0
                    , blur = shadowY * 1.3
                    , color = blackTranslucent
                    }

                SubmitPressed ->
                    { offset = ( 0, 0 )
                    , size = 0
                    , blur = 0
                    , color = blackTranslucent
                    }

                Submitted ->
                    { offset = ( 0, 0 )
                    , size = 0
                    , blur = 0
                    , color = blackTranslucent
                    }

        labelText =
            case model.submitButtonState of
                Submitted ->
                    "Thank you for your message!"

                _ ->
                    "SUBMIT"

        letterSpacing =
            case model.submitButtonState of
                SubmitHovered ->
                    toFloat fontSize * 0.12

                _ ->
                    toFloat fontSize * 0.1

        pads =
            { top = padSm model
            , bottom = padSm model
            , left = padLg model
            , right = padLg model
            }

        fontSize =
            fontMd model

        label =
            el
                [ Font.family sansSerifBold
                , Font.size fontSize
                , Font.letterSpacing letterSpacing
                , Font.color fontColor
                ]
            <|
                text labelText
    in
    Input.button
        [ centerX
        , paddingEach pads
        , Background.color backgroundColor
        , Border.shadow borderShadow
        , Events.onMouseDown <| UpdateSubmit PressSubmit
        , Events.onMouseUp <| UpdateSubmit UnpressSubmit
        , Events.onMouseEnter <| UpdateSubmit HoverSubmit
        , Events.onMouseLeave <| UpdateSubmit LeaveSubmit
        , focused []
        ]
        { onPress = Just NoOp
        , label = label
        }


contactFormPlaceholder : String -> Model -> Maybe (Input.Placeholder Msg)
contactFormPlaceholder placeholderText model =
    Just <|
        Input.placeholder
            [ height <| px <| fontXxl model
            , alignLeft
            ]
        <|
            el
                [ Font.family sansSerif
                , Font.size <| fontMd model
                , Font.color darkGrey
                ]
            <|
                text placeholderText


formInputStyle : Model -> List (Attribute Msg)
formInputStyle model =
    [ Font.alignLeft
    , Font.family sansSerif
    , Font.size <| fontMd model
    , Font.color darkGrey
    , Border.width 2
    , Border.color white
    , focused [ Border.color darkBrown ]
    ]


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


green : Color
green =
    rgb255 0 230 0


red : Color
red =
    rgb255 230 0 0


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


sansSerifBold : List Font.Font
sansSerifBold =
    [ Font.external
        { name = "Roboto Bold"
        , url = "https://fonts.googleapis.com/css2?family=Roboto:wght@700&display=swap"
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
