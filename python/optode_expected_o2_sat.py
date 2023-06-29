import math

def optode_expected_o2_sat(temp, apress):
    # cm3 dm-3 Garcia & Gordon coefficents
    A0 = 2.00907
    A1 = 3.22014
    A2 = 4.05010
    A3 = 4.94457
    A4 = -0.256847
    A5 = 3.88767
    B0 = -0.00624523
    B1 = -0.00737614
    B2 = -0.0103410
    B3 = -0.00817083
    C0 = -4.88682E-07
    salinity = 0 # as per default optode settings
    
    Ts = math.log((298.15-temp)/(273.15+temp))
    Csat = A0+(A1*Ts)+(A2*Ts^2)+(A3*Ts^3)+(A4*Ts^4)+(A5*Ts^5)+ salinity*(B0+(B1*Ts)+(B2*Ts^2)+(B3*Ts^3))+ (C0*salinity^2)
    pH2Osat = 1013.25*(math.exp(24.4543-(67.4509*(100/(temp+273.15)))-(4.8489 * math.log(((273.15+temp)/100)))-0.000544*salinity))
    Csat = math.exp(Csat) * (apress-pH2Osat)/(1013.25-pH2Osat)
    return(Csat)
