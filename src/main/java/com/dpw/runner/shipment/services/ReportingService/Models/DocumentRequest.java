package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class DocumentRequest implements IRunnerRequest {

    private Object data;
    private Options options = new Options();

    @Data
    public static class Options {

        private String type = "pdf";
        private boolean upload = false;
        public Options() {
            type = "pdf";
            upload = false;
        }
    }
}
