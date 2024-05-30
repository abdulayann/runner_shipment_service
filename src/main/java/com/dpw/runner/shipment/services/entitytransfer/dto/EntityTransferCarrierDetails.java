package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Map;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferCarrierDetails implements IEntityTranferBaseEntity {
    public String shippingLine;
    public String vessel;
    public String voyage;
    public String flightNumber;
    public String aircraftType;
    public String aircraftRegistration;
    public String truckRefNumber;
    public String journeyNumber;
    public String journeyRefNumber;
    public String origin;
    public String destination;
    public String originPort;
    public String destinationPort;
    public LocalDateTime eta;
    public LocalDateTime etd;
    public LocalDateTime ata;
    public LocalDateTime atd;
    private Map<String, EntityTransferMasterLists> masterData;
    private Map<String, EntityTransferUnLocations> unlocationData;
    private Map<String, EntityTransferCarrier> carrierMasterData;
    private Map<String, EntityTransferVessels> vesselsMasterData;
}
