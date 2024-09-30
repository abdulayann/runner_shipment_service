package com.dpw.runner.booking.services.dto.response.OrderManagement;

import lombok.Data;

import java.io.Serializable;

@Data
public class OrderContainerResponse implements Serializable {
    private String containerId;
    private String type;
    private Integer count;
    private String containerNumber;
}
