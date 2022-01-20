from dash import dcc
from dash import html
from requests.api import options
class CCDropdown:

    __slots__ = ['id', 'label', 'full_label', 'component', 'button', 'options_kv', 'options', 'previous_n_clicks', 'value', 'style']

    def __init__(self, id, label, options_kv, style=None):
        self.id = id
        self.label = options_kv[0][1]
        self.value = options_kv[0][0]
        self.style = style
        self.options_kv = options_kv
        self.options = {o[0] : self.__get_button(o) for o in options_kv}
        self.reload_label()
        self.button = html.Button(className = 'dropdown-button', id = self.id+'_button', children=self.full_label)
        self.component = self.create_component()
        self.previous_n_clicks = {o[0] : 0 for o in options_kv}

    def __get_button(self, option_kv):
        return html.Button(className='dropdown-option', id = self.id+'_'+option_kv[0], children=option_kv[1], n_clicks=0)

    def reload_label(self):
        self.full_label = html.Div(children=[
            html.Div(className='dropdown-label-text', children = [self.label]),
            html.Div(className='dropdown-label-arrow', children = ["▼"]),
        ], style = {'width': '100%', 'height': '100%', 'display': 'flex', 'justify-content': 'center'})

    def create_component(self):
        # style={'max-height': f'{len(self.options_kv) * 50}px'}
        component =  html.Div(className='dropdown-div', id = self.id,
        children = [
            self.button,
            html.Div(className = 'dropdown-content',
                children=list(self.options.values()),
                style = self.style)
        ])
        self.options[self.options_kv[-1][0]].style={'border-radius': '0 0 25px 25px'}
        return component

    def get_changed(self, args):
        keys = list(self.options.keys())
        for i in range(len(keys)):
            if self.previous_n_clicks[keys[i]] != args[i] and args[i] != 0: return i, True
        return -1, False

    def on_click(self, args):
        keys = list(self.options.keys())
        i, value_changed = self.get_changed(args)
        self.previous_n_clicks[keys[i]] = args[i]
        if (not value_changed): 
            return None, None
        self.label = self.options_kv[i][1]
        self.value = keys[i]
        return keys[i], self.options_kv[i][1]
