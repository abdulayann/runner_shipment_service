package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import lombok.*;

import java.util.ArrayList;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@ToString
public class AutoAttachConsolidationV3Request extends ListCommonRequest {
    private String transportMode;
    private String direction;
    private Long shipId;
    private ArrayList<Long> branchIds;
}
