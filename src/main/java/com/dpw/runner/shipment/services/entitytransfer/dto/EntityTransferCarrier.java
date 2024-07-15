package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.io.Serializable;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferCarrier implements IEntityTranferBaseEntity, Serializable {
    public String ItemValue;
    public String ItemDescription;
    public String Email;
    public String CarrierContactPerson;
    public String ValuenDesc;
    public String Cascade;
    public String Identifier1;
    public String Identifier2;
    public String Identifier3;
    public String TransportCodeDescription;
    public String IATACode;
    public String AirlineCode;
    public Boolean HasSeaPort;
    public Boolean HasAirPort;
    public String AirLinePrefixValue;
    public String CarriersLogo;
    public String ValuenDescAir;
    public String HeadQuartersDetails;
}
