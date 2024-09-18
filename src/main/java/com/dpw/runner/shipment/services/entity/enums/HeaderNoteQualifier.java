package com.dpw.runner.shipment.services.entity.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum HeaderNoteQualifier {
    D("Direct"), C("Consolidation"), L ("Letter of Credit");

    private final String value;
}
