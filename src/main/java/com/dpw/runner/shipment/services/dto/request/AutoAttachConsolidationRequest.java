package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import lombok.*;

import java.time.LocalDateTime;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@ToString
public class AutoAttachConsolidationRequest extends ListCommonRequest {
    private String masterBill;
    private String transportMode;
    private String voyageNumber;
    private String vessel;
    private String pol;
    private String pod;
    private LocalDateTime eta;
    private LocalDateTime etd;
    private PartiesRequest client;
    private PartiesRequest consignee;
    private PartiesRequest consigner;
    private String direction;
    private String shipmentType;
    private Long shipId;
}
