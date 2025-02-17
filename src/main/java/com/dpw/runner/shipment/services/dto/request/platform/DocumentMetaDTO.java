package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class DocumentMetaDTO implements Serializable {
    private String name;
    private String document_type;
    private String document_link;
    private String uploaded_by_user_id;
}
