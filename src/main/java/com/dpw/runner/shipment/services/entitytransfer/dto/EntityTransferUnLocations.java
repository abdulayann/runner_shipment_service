package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.io.Serializable;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferUnLocations implements IEntityTranferBaseEntity, Serializable {
    public String Country;
    public String LocCode;
    public String Name;
    public String NameWoDiacritics;
    public String SubDiv;
    public String Coordinates;
    public String lookupDesc;
    public String lookupDescAir;
    public String lookupDescSea;
    public String IATACode;
    public String IATARegionCode;
    public String PortName;
    public String AirPortName;
    public String SeaPortName;
    public String State;
    public String TimeZone;
    public Boolean HasPost;
    public Boolean HasRail;
    public Boolean HasDischarge;
    public Boolean HasCustoms;
    public Boolean HasRoad;
    public Boolean HasSeaPort;
    public Boolean HasUnload;
    public Boolean HasStore;
    public Boolean HasOutPort;
    public Boolean HasAirport;
    public Boolean HasTerminal;
    public String CountryThreeDigitCode;
    public String LocationsReferenceGUID;
}
