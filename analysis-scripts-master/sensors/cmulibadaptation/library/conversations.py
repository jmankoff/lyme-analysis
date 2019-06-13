import pandas as pd
from sqlalchemy.types import *
# YSS ---- this does not work when there are multiple packages all with a module named utils
# from utils.setting import AUDIO_SAMPLE_RATE_IN_SECONDS
# ---- YSS
from .utils.setting import AUDIO_SAMPLE_RATE_IN_SECONDS

# YSS bad bad practice to use column names that do not match that of AWARE DB tables

def number_samples_conversations(g):
    if g is None:
        return None
    return len(g)

def number_of_conversations(g):
    if g is None or len(g) == 0:
        return None
    return g[g["convo_start"] != 0]["convo_start"].count()

def length_of_conversations_seconds(g):
    if g is None or len(g) == 0:
        return None
    wconvo= g[g["convo_start"] != 0]
    return int((wconvo["convo_end"] - wconvo["convo_start"]).sum())

def mean_voice_energy(g):
    if g is None or len(g) == 0:
        return None
    voice = g[g["inference"] == 1]
    return voice["energy"].mean()

def std_voice_energy(g):
    if g is None or len(g) == 0:
        return None
    voice = g[g["inference"] == 1]
    return voice["energy"].std()

def max_voice_energy(g):
    if g is None or len(g) == 0:
        return None
    voice = g[g["inference"] == 1]
    return voice["energy"].max()

def min_voice_energy(g):
    if g is None or len(g) == 0:
        return None
    voice = g[g["inference"] == 1]
    return voice["energy"].min()

def mean_noise_energy(g):
    if g is None or len(g) == 0:
        return None
    noise = g[g["inference"] == 2]
    return noise["energy"].mean()

def std_noise_energy(g):
    if g is None or len(g) == 0:
        return None
    noise = g[g["inference"] == 2]
    return noise["energy"].std()

def max_noise_energy(g):
    if g is None or len(g) == 0:
        return None
    noise = g[g["inference"] == 2]
    return noise["energy"].max()

def min_noise_energy(g):
    if g is None or len(g) == 0:
        return None
    noise = g[g["inference"] == 2]
    return noise["energy"].min()

def percentage_silence_total_with_unknown(g):
    if g is None or len(g) == 0:
        return None
    # g = resampleseparatedays_sec(g, AUDIO_SAMPLE_RATE_IN_SECONDS)
    silence = g[g["inference"] == 0]["inference"]
    total = g.loc[g['inference'].isin([0,1,2,3])]["inference"]
    if total.count() == 0: return 0
    return (float(silence.count())/total.count())

def percentage_silence_total(g):
    if g is None or len(g) == 0:
        return None
    # g = resampleseparatedays_sec(g, AUDIO_SAMPLE_RATE_IN_SECONDS)
    silence = g[g["inference"] == 0]["inference"]
    total = g.loc[g['inference'].isin([0,1,2])]["inference"]
    if total.count() == 0: return 0
    return (float(silence.count())/total.count())

def percentage_voice_total_with_unknown(g):
    if g is None or len(g) == 0:
        return None
    # g = resampleseparatedays_sec(g, AUDIO_SAMPLE_RATE_IN_SECONDS)
    voice = g[g["inference"] == 1]["inference"]
    total = g.loc[g['inference'].isin([0,1,2,3])]["inference"]
    if total.count() == 0: return 0
    return (float(voice.count())/total.count())

def percentage_voice_total(g):
    if g is None or len(g) == 0:
        return None
    # g = resampleseparatedays_sec(g, AUDIO_SAMPLE_RATE_IN_SECONDS)
    voice = g[g["inference"] == 1]["inference"]
    total = g.loc[g['inference'].isin([0,1,2])]["inference"]
    if total.count() == 0: return 0
    return (float(voice.count())/total.count())

def percentage_noise_total_with_unknown(g):
    if g is None or len(g) == 0:
        return None
    # g = resampleseparatedays_sec(g, AUDIO_SAMPLE_RATE_IN_SECONDS)
    noise = g[g["inference"] == 2]["inference"]
    total = g.loc[g['inference'].isin([0,1,2,3])]["inference"]
    if total.count() == 0: return 0
    return (float(noise.count())/total.count())

def percentage_noise_total(g):
    if g is None or len(g) == 0:
        return None
    # g = resampleseparatedays_sec(g, AUDIO_SAMPLE_RATE_IN_SECONDS)
    noise = g[g["inference"] == 2]["inference"]
    total = g.loc[g['inference'].isin([0,1,2])]["inference"]
    if total.count() == 0: return 0
    return (float(noise.count())/total.count())


CONVERSATION_APPLY = [
    number_samples_conversations,
    number_of_conversations,
    length_of_conversations_seconds,
    mean_voice_energy,
    std_voice_energy,
    max_voice_energy,
    min_voice_energy,
    mean_noise_energy,
    std_noise_energy,
    max_noise_energy,
    min_noise_energy,
    percentage_silence_total,
    percentage_silence_total_with_unknown,
    percentage_voice_total,
    percentage_voice_total_with_unknown,
    percentage_noise_total,
    percentage_noise_total_with_unknown,
]

CONVERSATION_SQL_TYPES = {
    "number_samples_conversations": Integer,
    "number_of_conversations" : Integer,
    "length_of_conversations_seconds" : Integer,
    "mean_voice_energy" : Float,
    "std_voice_energy" : Float,
    "max_voice_energy": Float,
    "min_voice_energy": Float,
    "mean_noise_energy" : Float,
    "std_noise_energy" : Float,
    "max_noise_energy": Float,
    "min_noise_energy": Float,
    "percentage_silence_total": Float,
    "percentage_silence_total_with_unknown": Float,
    "percentage_voice_total": Float,
    "percentage_voice_total_with_unknown": Float,
    "percentage_noise_total": Float,
    "percentage_noise_total_with_unknown": Float,
}
