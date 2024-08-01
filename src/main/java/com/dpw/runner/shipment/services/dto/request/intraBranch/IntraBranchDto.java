package com.dpw.runner.shipment.services.dto.request.intraBranch;

import lombok.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class IntraBranchDto {
    private boolean isIntraBranch;
    private boolean isHub;

    private List<Long> tenantIds;
}
