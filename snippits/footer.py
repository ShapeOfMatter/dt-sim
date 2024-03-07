retval = procedure(inputs.reshape(-1, 1), tapes.reshape(-1, 1), 1)
print("PyTrace{inputs = [" + ",".join(str(int(v[0])) for v in retval.inputs) + "], tapes = [" + ",".join(str(int(v[0])) for v in retval.tapes) + "], outputs = [" + ",".join(str(int(v[0])) for v in retval.outputs) + "], views = [" + ",".join(str(int(v[0])) for v in retval.views) + "]}")
