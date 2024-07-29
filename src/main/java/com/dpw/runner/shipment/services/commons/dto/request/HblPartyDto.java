package com.dpw.runner.shipment.services.commons.dto.request;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;
import java.util.UUID;


@Data
@Builder
@ApiModel("Hbl Parties Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblPartyDto implements Serializable {
    private Long id;
    private UUID guid;
    private Boolean isShipmentCreated;
    private String type;
    private String orgCode;
    private Integer orgId;
    private String name;
    private String email;
    private String address;
    private String taxId;
}