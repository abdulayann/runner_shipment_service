package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.UUID;


@Data
@Builder
@ApiModel("Hbl Parties Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblPartyDto extends CommonRequest {
    private Long id;
    private UUID guid;
    private String type;
    private String OrgCode;
    private Integer OrgId;
    private String Name;
    private String Email;
    private String Address;
    private String TaxId;
}