package com.dpw.runner.shipment.services.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.io.Serializable;
import java.util.UUID;


@Data
@Builder
@Schema(description = "Hbl Parties Model")
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
    private String address1;
    private String address2;
    private String city;
    private String state;
    private String zipCode;
    private String country;
}