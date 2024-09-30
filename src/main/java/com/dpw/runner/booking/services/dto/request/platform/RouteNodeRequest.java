package com.dpw.runner.booking.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
public class RouteNodeRequest {
    private String order;
    private String code;
    private String name;

}

