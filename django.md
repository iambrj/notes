# Tutorial 1
* `django-admin startproject polls` - create a new django project named `polls`
* `python manage.py startserver` - start web app on localhost
# Creating a new view
1. `pollsapp/views.py` - define `index(request)`
2. `pollsapp/urls.py` - add url for view here
```
urlpatterns = [
        path('', views.index, name='index'),
        ]
```
3. `pollsproject/urls.py` - add rootURL conf herer
```
urlpatterns = [
    path('pollsapp/', include('pollsapp.urls')), # new url
    path('admin/', admin.site.urls),
]
```
The `path` function here takes four arguments
    - `route`: string containing URL pattern 
    - `view`: function to be called when pattern is matched
    - `name`: unique identifier for URL
# Tutorial 2
* `INSTALLED_APPS` in `settings.py` holds list of all django applications that
    are activated
* `python manage.py createsuperuser` - create new admin
* `pollsapp/admin.py` - contains models we want to be displayed in our admin
    interface
```
from django.contrib import admin

from .models import Question

admin.site.register(Question)
```
## Making model changes
* Change your models (in models.py).
* Run python manage.py makemigrations to create migrations for those changes
* Run python manage.py migrate to apply those changes to the database.
