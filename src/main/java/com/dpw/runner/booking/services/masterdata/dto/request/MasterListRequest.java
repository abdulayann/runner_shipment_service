package com.dpw.runner.booking.services.masterdata.dto.request;

import lombok.*;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MasterListRequest {
    public String ItemValue;
    public String ItemType;
    public String Cascade;
}
