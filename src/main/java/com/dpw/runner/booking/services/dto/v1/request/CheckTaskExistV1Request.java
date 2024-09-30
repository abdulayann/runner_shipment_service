package com.dpw.runner.booking.services.dto.v1.request;

import lombok.*;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class CheckTaskExistV1Request {
    private List<Integer> sendToBranch;
    private List<String> sendToOrg;
    private String consoleId;
    private String shipId;
    private String entityType;
}
