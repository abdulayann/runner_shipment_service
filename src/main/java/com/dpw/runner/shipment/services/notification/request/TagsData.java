package com.dpw.runner.shipment.services.notification.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TagsData implements Serializable {
    private String tagName;
    private String tagValue;
}
