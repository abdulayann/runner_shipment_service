package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class DocumentMetaDTO {
    private String name;
    private String document_type;
    private String document_link;
    private String uploaded_by_user_id;
}
