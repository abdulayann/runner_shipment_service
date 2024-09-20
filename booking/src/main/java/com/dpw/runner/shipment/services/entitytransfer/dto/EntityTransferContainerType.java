package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.io.Serializable;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferContainerType implements IEntityTranferBaseEntity, Serializable {
    public String Code;
    public String Description;
    public String Mode;
    public String ContainerType;
    public Double Teu;
    public String IATAClass;
    public String Size;
    public Boolean Active;
    public Boolean IsTank;
    public Boolean IsReeferTempControlled;
    public String TextField;
    public Boolean CheckForExistingContainerType;
    public Double TareWeight;
    public String TareWeightUnit;
    public Double MaxGrossWeight;
    public String MaxGrossWeightUnit;
    public Double MaxCargoGrossWeight;
    public String MaxCargoGrossWeightUnit;
    public Double NetWeight;
    public String NetWeightUnit;
    public Double CubicCapacity;
    public String CubicCapacityUnit;
    public Double InnerLength;
    public Double InnerBreadth;
    public Double InnerHeight;
    public String InnerMeasurementUnit;
    public Double OuterLength;
    public Double OuterBreadth;
    public Double OuterHeight;
    public String OuterMeasurementUnit;
    public String IsoCode;
    public String IntegrationCode;
}