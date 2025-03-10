from typing import Iterable


def interrupt_on_error(error_msg: str):
    print("Error: " + error_msg)
    exit(-1)


def only_reachable_code(stmts):
    reachable = []
    for stmt in stmts:
        new_stmt = stmt.get_reachable_only()

        if isinstance(new_stmt, Iterable):
            for ns in new_stmt:
                reachable.append(ns)
        else:
            reachable.append(new_stmt)

    return reachable
