Usage
=====

The first execution probably yields the usage.

    % htodo
    usage
    htodo add: add a task
    htodo:     pick a task

There is a way to add a task.

    % htodo add
    a task to: drink cofee

There is a way to see one task.

    % htodo
    drink cofee
      d: done
      n: never doing
      w: waiting for another task

Here, you remember you have to get cofee.

    > w
    1: drink cofee
    n: a new task
    after which task?
    > n
    a task to: get cofee

Next, you ask the system what to do.

    % htodo
    get cofee
      d: done
      n: never doing
      w: waiting for another task

The system tells you to get cofee, so you do it.

    > d

Again, you ask the system what to do.

    % htodo
    drink cofee
      d: done
      n: never doing
      w: waiting for another task

Now you drink the cofee.

    > d
