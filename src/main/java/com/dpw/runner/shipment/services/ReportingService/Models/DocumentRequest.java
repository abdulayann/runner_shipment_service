package com.dpw.runner.shipment.services.ReportingService.Models;

import lombok.Data;

@Data
public class DocumentRequest {

    private Object data;
    private Options options = new Options();

    @Data
    public class Options {

        public Options() {
            type = "pdf";
            upload = false;
        }
        private String type = "pdf";
        private boolean upload = false;
    }
}
