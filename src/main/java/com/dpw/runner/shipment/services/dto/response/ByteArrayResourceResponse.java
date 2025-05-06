package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.core.io.ByteArrayResource;

public class ByteArrayResourceResponse extends ByteArrayResource implements IRunnerResponse { //NOSONAR
    public ByteArrayResourceResponse(byte[] byteArray) {
        super(byteArray);
    }
}
