- JSX : an extension to javascript that provides a way to structure component
    rendering. For example
    ```
    const element = <h1> Hello, world! </h1>;
    ```
    JSX gets compiled into regular javascript function calls and evaluate to
    javascript objects
- Babel does this compilation of JSX to `React.createElement()` calls. For
    instance
    ```
    const element = (
        <h1 className="greeting">
            Hello, world!
        </h1>
    );
    ```
    is the same as
    ```
    const element = React.createElement(
        'h1',
        {className: 'greeting'},
        'Hello, world!'
    );
    ```
    and `React.createElement` performs checking and returns an object as follows
    ```
    const element = {
        type: 'h1',
        props : {
            className : 'greeting',
            children : 'Hello, world!'
        }
    };
    ```
    React reads these objects and converts them to DOM. This can be done with a
    call to `ReactDOM.render()` as follows
    ```
    ReactDOM.render(element, document.getElementById('root'));
    ```
- Function components
    1. Accept single object argument, `props`
    2. Returns React element

    For example
    
    ```
    function Welcome(props) {
        return <h1> Hello, {props.name}</h1>
    }
    ```
- Class components
    1. Extend the `React.Component` class
    2. Overload the `render()` function
    ```
    class Welcome extends React.Component {
        render() {
            return <h1> Hello, {this.props.name} </h1>
        }
    }
    ```
- These user defined components can be rendered as follows
    ```
    const element = <Welcome name="Sara" />;
    ReactDOM.render(
        element,
        document.getElementById('root')
    );
    ```
- State and lifecycle
    ```
    class Clock extends React.Component {
      constructor(props) {
        super(props); // constructor should initialize superclass
        this.state = {date: new Date()}; // constructor sets initial values of state
      }

      // Lifecycle management, gets called when the component is rendered
      componentDidMount() {
        this.timerID = setInterval(
          () => this.tick(),
          1000
        );
      }

      // Lifecycle management, gets called when the component is removed
      componentWillUnmount() {
        clearInterval(this.timerID);
      }

      tick() {
        this.setState({
          date: new Date()
        });
      }

      render() {
        return (
          <div>
            <h1>Hello, world!</h1>
            <h2>It is {this.state.date.toLocaleTimeString()}.</h2>
          </div>
        );
      }
    }

    ReactDOM.render(
      <Clock />,
      document.getElementById('root')
    );
    ```
