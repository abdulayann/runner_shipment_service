package com.dpw.runner.shipment.services.ReportingService.Models.Commons;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.notification.request.TagsData;
import lombok.Data;

import java.util.List;

@Data
public class EmailBodyResponse implements IRunnerResponse {
    private String body;
    private String subject;
    private String to;
    private String cc;
    private List<TagsData> tags;

}
