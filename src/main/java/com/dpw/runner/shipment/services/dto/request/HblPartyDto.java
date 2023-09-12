package com.dpw.runner.shipment.services.dto.request;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.UUID;


@Data
@Builder
@ApiModel("Hbl Parties Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblPartyDto {
    private Long id;
    private UUID guid;
    private String type;
    private String orgCode;
    private Integer orgId;
    private String name;
    private String email;
    private String address;
    private String taxId;
}