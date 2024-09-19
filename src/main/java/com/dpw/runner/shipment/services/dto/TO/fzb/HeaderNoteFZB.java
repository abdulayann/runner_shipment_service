package com.dpw.runner.shipment.services.dto.TO.fzb;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class HeaderNoteFZB {

    @Size(max = 1, message = "contentCode exceeds the maximum size")
    private String contentCode;

    @Size(max = 70, message = "content exceeds the maximum size")
    private String content;
}
