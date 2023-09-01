package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

@Data
public class NoteRequestV2 {
    private String InsertUserDisplayName;
    private Boolean Isprivate;
    private String Text;
}
