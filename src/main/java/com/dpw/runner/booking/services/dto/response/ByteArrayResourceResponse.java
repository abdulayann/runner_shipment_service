package com.dpw.runner.booking.services.dto.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import org.springframework.core.io.ByteArrayResource;

public class ByteArrayResourceResponse extends ByteArrayResource implements IRunnerResponse {
    public ByteArrayResourceResponse(byte[] byteArray) {
        super(byteArray);
    }
}
